#include "buffer.h"
#include "command.h"
#include "config.h"

int main(int argc, char **argv)
{
	Config config = {*argv, stdin, stdout};
	Buffer buffer = {0};
	Command cmd = {0};

	args_process(&config, argc, argv);
	if (config.output_stream_name)
		buffer_load(&buffer, &config);

	for (int exit = 0; !exit; )
		exit = cmd_process(&cmd, &buffer, &config);

	buffer_clean(&buffer, &config);
	return 0;
}
