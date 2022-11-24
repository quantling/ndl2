#include <streambuf>
#include <iosfwd>
#include <cstdlib>
#include <vector>
#include <omp.h>

class ndlOutBuffer : public std::streambuf {
	private:
		std::ostream &rStream;
		std::vector<char> buffer;
    omp_lock_t bufferLock;

	public:
		explicit ndlOutBuffer(std::ostream &rStream, std::size_t bufferSize=1024);
		~ndlOutBuffer();
	
	private:
		int_type overflow(int_type ch);
//    std::streamsize xsputn(const char* p, std::streamsize n);
		int sync();

		// copying and assignment not allowed, not implemented
		ndlOutBuffer(const ndlOutBuffer &);
		ndlOutBuffer &operator= (const ndlOutBuffer &);
};
