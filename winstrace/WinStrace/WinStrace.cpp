#include <iostream>
#include <windows.h>

int main(int argc, char *argv[]) 
{
	HANDLE h = CreateFile(L"foo.txt", GENERIC_READ, 0, NULL, CREATE_ALWAYS, 0, 0);
	std::cout << h << std::endl;
	CloseHandle(h);
	return 0;
}
