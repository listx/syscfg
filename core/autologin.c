#include <unistd.h>
/* Log in as a user. */

int main(void) {
	int ret = 0;
	ret = execlp( "login", "login", "-f", "l", 0);
	return ret;
}
