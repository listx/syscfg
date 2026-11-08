-- Copyright 2024 Linus Arver
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--      http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.

-- FIXME: Make LUA_PATH also include $HOME/.melby/?.lua.
-- FIXME: Make melbyc use the above path as a default if it's there, and only
-- overwrite it with the FILEPATH argument (move this FILEPATH argument to an
-- optional, not required, flag).
local Git = require "Git"
local KubeCurrent = require "KubeCurrent"
local Kube = require "Kube"

Config = {}
Config["render_options"] = {
  format = "RENDER_FORMAT_UNIX_TERMINAL",
  color_depth = "RENDER_COLOR_DEPTH_24_BIT"
}

-- This is required for both verification and reducing the inputs (env vars) to
-- a known subset.
-- FIXME: Also, strip the given env vars coming from melbyc down to only these
-- env vars. This forces the user to be explicity up-front (at this level)
-- exactly which env vars they expect to use.
-- FIXME: Require that these env vars are specified as keys for each element in
-- Config.validation's env_vars
Config.view_params_types = {
  env={
    HOME                      = {"required", "path"},
    HOST                      = {"required", "string"},
    -- "paths" means 1 or more colon-separated paths (no colons if only 1 path).
    -- We don't need to bother with a real file on disk because we will never
    -- actually read this path; for validation
    KUBECONFIG                = {"required", "paths"},
    MELBY_DIR                  = {"required", "path"},
    MELBY_LAST_CMD_EXIT_STATUS = {"required", "int"},
    -- "pathblob" is like "path", except that it represents the *contents* of a
    -- path. The filename is generated automatically using a temp folder plus
    -- the name of the env var.
    MELBY_PATH_ALIASES_FILE    = {"required", "pathblob"},
    MELBY_TIME_ZONE            = {"optional", "string"},
    MELBY_UNIX_SECONDS         = {"optional", "uint"},
    MELBY_ZSH_KEYMAP_INDICATOR = {"required", "string"},
    PWD                       = {"required", "path"},
    USER                      = {"required", "string"},
  },
  shell_pid                   = {"required", "uint"}
}

-- Return an array of array of env var overrides and substrings. For the
-- substrings, these are substrings that must be present in the output of the
-- Config.view() function.
--
-- We have to override some env vars because otherwise they become polluted with
-- real paths that are coming from the actual melbyc invocation. For the
-- "preview" (fake) environment we want to control, ideally, all the env vars in
-- allowed_env_vars...!
--
-- FIXME: Set all env vars in allowed_env_vars to make it deterministic. Also
-- probably set the shell_pid as well to make the function truly pure.
-- FIXME: rename env_var_overrides to something else because we're overriding
-- _every_ env var that matters to us.
Config.view_tests = {
  -- time_idx 0
  {env={
    HOME                      = "/home/alice",
    HOST                      = "laptop",
    KUBECONFIG                = "/home/alice/.kube/config",
    -- We don't actually need to have a real /home/alice/.melby folder with other
    -- Lua files in it, because by the time this entire config is read, we
    -- already parsed in all the Lua files we need. So this MELBY_DIR is really
    -- just a placeholder.
    MELBY_DIR                  = "/home/alice/.melby",
    MELBY_LAST_CMD_EXIT_STATUS = "0",
    -- this is a "blobpath". the actual value here used in the env var will
    -- point to a temporary filename which has the contents here.
    MELBY_PATH_ALIASES_FILE    =
        [[
            # Kubernetes.
            hash -d   ki=${HOME}/go/src/github.com/kubernetes/k8s.io
            hash -d   kk=${HOME}/go/src/k8s.io/kubernetes
            hash -d   kr=${HOME}/go/src/k8s.io/release
            hash -d   kt=${HOME}/go/src/k8s.io/test-infra
            hash -d  cip=${HOME}/go/src/sigs.k8s.io/k8s-container-image-promoter

            # Notes and dotfiles.
            hash -d    l=${HOME}/lo
            hash -d    s=${HOME}/syscfg
        ]],
    MELBY_TIME_ZONE            = "America/Los_Angeles",
    MELBY_UNIX_SECONDS         = "1679032715",
    MELBY_ZSH_KEYMAP_INDICATOR = "I",
    PWD                       = "/repo/a",
    USER                      = "alice",
   },
  -- FIXME: move shell_pid to an env_var to unify all inputs into env_vars.
  shell_pid                   = "54311",
  expected_substrings={"K8s...", "Git..."}},
}

function Config.view (env, shell_pid)
  local widgets_ps1_line1 = {}
  local widgets_ps1_line2 = {}
  -- FIXME: We need a fake version of get_path_aliases as well, because we don't
  -- want to actually read from disk, but let the user instead specify the
  -- *contents* of such a file.
  local path_aliases = melbyd.get_path_aliases(env["MELBY_PATH_ALIASES_FILE"])

  local pwd = env["PWD"]
  local pwd_pretty_options = {}
  -- We try to fit under this length if possible.
  pwd_pretty_options["shorten_threshold"] = 30
  pwd_pretty_options["aliases"] = path_aliases
  pwd_pretty_options["env"] = env
  -- FIXME: Make melbyd.get_path_pretty return a LIST of strings that should be
  -- joined with the path separator ("/"). Each element can have fields such as
  -- color and type (whether it's a regular folder or symlink) as well as the
  -- owner and permissions on it. For the first element (possibly an alias), we
  -- can have a field to denote whether it's an alias. All of these bits of
  -- information can be colorized a different way to give semantic coloring to
  -- the final colorized path.
  local pwd_pretty_str = melbyd.get_path_pretty(pwd, pwd_pretty_options)

  -- Get the current time. In non-test-code, we can just call melbyd.get_time("%d
  -- C %H:%M:%S %Z")
  local unix_seconds = env["MELBY_UNIX_SECONDS"] or melbyd.get_unix_seconds()
  local tz = env["MELBY_TIME_ZONE"] or "LOCAL"
  local time_day_name = melbyd.get_time("%a", unix_seconds, tz)
  local day_in_kanji = {}
  day_in_kanji["Mon"] = utf8.char(0x6708) -- "月"
  day_in_kanji["Tue"] = utf8.char(0x706b) -- "火"
  day_in_kanji["Wed"] = utf8.char(0x6c34) -- "水"
  day_in_kanji["Thu"] = utf8.char(0x6728) -- "木"
  day_in_kanji["Fri"] = utf8.char(0x91d1) -- "金"
  day_in_kanji["Sat"] = utf8.char(0x571f) -- "土"
  day_in_kanji["Sun"] = utf8.char(0x65e5) -- "日"

  local time_day_name_kanji = day_in_kanji[time_day_name]
  local time_str = melbyd.get_time(" %m-%d " .. time_day_name_kanji ..
                                   " %H:%M:%S %Z ", unix_seconds, tz)
  local time = {str=time_str, fg="black", bg="gold", styles={"bold"}}
  table.insert(widgets_ps1_line1, time)

  local time_utc_day_name = melbyd.get_time("%a", env["MELBY_UNIX_SECONDS"],
                                            "Etc/UTC")
  local time_utc_day_name_kanji = day_in_kanji[time_utc_day_name]
  local time_utc_str = melbyd.get_time(" %m-%d " .. time_utc_day_name_kanji ..
                                       " %H:%M:%S %Z ", unix_seconds, "Etc/UTC")
  local time_utc = {str=time_utc_str, fg="black", bg="pink", styles={"bold"}}
  table.insert(widgets_ps1_line1, time_utc)

  local zki_str = env["MELBY_ZSH_KEYMAP_INDICATOR"] or "I"
  local zki = {str=" <" .. zki_str .. "> ", fg="white", styles={"bold"}}
  if zki_str == "N" then
    zki.fg = "black"
    zki.bg = "lightskyblue"
  end
  table.insert(widgets_ps1_line1, zki)

  local last_command_exit_status_str = env["MELBY_LAST_CMD_EXIT_STATUS"] or ""
  local last_command_exit_status = {str=" " .. last_command_exit_status_str .. " ",
                                    fg="black", bg="red", styles={"bold"}}
  if #last_command_exit_status_str > 0 and last_command_exit_status_str ~= "0" then
    table.insert(widgets_ps1_line1, last_command_exit_status)
  end

  -- Create a one-shot message for testing (when we want to publish an ad-hoc
  -- message).
  --
  --local message = {topic="srs_Git", from="/home/l/prog/melby", payload={level="info", text="Hello world"}}
  --
  --melbyd.broadcast("srs_Git", message)

  user = env["USER"] or "user"
  host = env["HOST"] or "host"
  user_host = {str=(user .. "@" .. host)}
  table.insert(widgets_ps1_line1, user_host)

  pwd_pretty = {str=pwd_pretty_str, fg="cyan", styles={"bold"}}
  table.insert(widgets_ps1_line1, pwd_pretty)

  -- FIXME: Run Kube.read() for every Kubernetes context found in the set of
  -- MELBY_KUBE_CONTEXTS.
  --
  -- get_kube_contexts(env["MELBY_KUBE_CONTEXTS"])
  -- example: "ctx1,ctx2,ctx3"
  --
  -- FIXME: Need to loop this around multiple context:namespace combos, probably
  -- encoded in MELBY_KUBE_CONTEXTS
  --
  --local kube_standard_resource = Kube:read("kind-kind-prow-integration",
  --"default", env["MELBY_DIR"])
  --
  --local kube_widgets = Kube.view(kube_standard_resource)
  --for _, w in ipairs(kube_widgets) do
  --  table.insert(widgets_ps1_line2, w)
  --end

  local kubecurrent_standard_resource = KubeCurrent:read(env,
                                                         {"KUBECONFIG", "HOME"})
  local health_resource = {}
  local health_resource_status = ""
  local health_widgets = {}
  local health_str = ""
  if kubecurrent_standard_resource.status ==
       "STANDARD_RESOURCE_STATUS_LOADED" then
    health_resource = Kube:read(kubecurrent_standard_resource.kvs.context,
                                kubecurrent_standard_resource.kvs.namespace,
                                env["MELBY_DIR"])
    health_resource_status = health_resource.status
    health_widgets = Kube.view(health_resource)
    health_str = melbyd.render(health_widgets, {str=" "})
  end

  local kubecurrent_widgets = KubeCurrent.view(kubecurrent_standard_resource,
                                               health_resource.status,
                                               health_str)
  for _, w in ipairs(kubecurrent_widgets) do
    table.insert(widgets_ps1_line2, w)
  end

  -- Note that because Git:read() returns very quickly (we designed the backend
  -- to be asynchronous), we don't need to worry about parallelism at the Lua
  -- configuration level.
  --
  -- Get git information for PWD, if any. We use "MELBY_DIR" so that we can use
  -- it in the Git.readers() function for calling custom scripts at custom
  -- locations (wherever MELBY_DIR points to).
  local git_standard_resource = Git:read(env, {"PWD", "MELBY_DIR"})
  -- Use the standard widgets that come with the "Git.lua" module.
  local git_widgets = Git.view(git_standard_resource)
  for _, w in ipairs(git_widgets) do
    table.insert(widgets_ps1_line2, w)
  end

  -- Render widgets. Some go into line 1, others into line 2.
  local ps1_line1 = melbyd.render(widgets_ps1_line1, {str=" "})
  local ps1_line2 = melbyd.render(widgets_ps1_line2, {str=" "})

  -- We need to get messages first because we need to subscribe first. Otherwise
  -- Git SRS will start first and we'll miss its first broadcast (how long the
  -- initial read took).
  local messages = melbyd.get_shell_messages(shell_pid,
                                            {Git, KubeCurrent},
                                            {PWD=env["PWD"],
                                             KUBECONFIG=(env["KUBECONFIG"] or "")})
  local message_strs = {}
  for _, message in ipairs(messages) do
    if message["topic"] == "srs_Git" then
      message_str = Git.message_to_string(message, pwd_pretty_options)
    elseif message["topic"] == "srs_KubeCurrent" then
      message_str = KubeCurrent.message_to_string(message)
    end
    table.insert(message_strs, message_str .. "\n")
  end

  -- Create a dictionary of shell variables to create.
  export = {}
  table.insert(export, {name="MELBY_PS1_LINE1", val=ps1_line1, type="string"})
  table.insert(export, {name="MELBY_PS1_LINE2", val=ps1_line2, type="string"})
  table.insert(export, {name="MELBY_SHELL_MESSAGES", val=message_strs,
                        type="array"})

  -- Create a shell script that instantiates the exported variables.
  script = melbyd.to_shell_script(export)
  return script
end

-- This line is to force evaluation of these loaded modules by Luerl, such that
-- they show up in the loaded vm state.
return {g = Git, kc = KubeCurrent, k = Kube}
