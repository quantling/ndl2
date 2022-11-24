#include "ndlOutBuffer.h"
#include <cctype>
#include <ostream>
#include <functional>
#include <iostream> //TODO remove after removing cerr
//#include <execinfo.h> //TODO
//#include <stdio.h> //TODO


ndlOutBuffer::ndlOutBuffer(std::ostream &rStream, std::size_t bufferSize) :
	rStream(rStream), buffer(bufferSize)
{
	rStream.clear();
	char *base = &buffer.front();
	setp(base, base + buffer.size());
  omp_init_lock(&bufferLock);
}

ndlOutBuffer::~ndlOutBuffer()
{
  omp_destroy_lock(&bufferLock);
}

ndlOutBuffer::int_type ndlOutBuffer::overflow(int_type ch)
{
  if (ch == traits_type::eof())
    return traits_type::eof();
  try {
    omp_set_lock(&bufferLock);
    buffer.resize(2*buffer.size());
    char *base = &buffer.front();
    setp(base, base + buffer.size());
    *pptr() = ch;
    pbump(1);
    omp_unset_lock(&bufferLock);
    return ch;
  } catch(...) {
    std::cerr << "<start cerr> overflow(): exception in buffer resize(?) (" << omp_get_thread_num() << ") <end cerr>" << std::endl << std::flush;
    omp_unset_lock(&bufferLock);
    return traits_type::eof();
  }
}

//std::streamsize ndlOutBuffer::xsputn(const char* p, std::streamsize n)
//{
//  omp_set_lock(&bufferLock);
//  std::streambuf::xsputn(p, n);
//  omp_unset_lock(&bufferLock);
//  if (omp_get_thread_num() == 0)
//    sync();
//  return n;
//}

int ndlOutBuffer::sync()
{
  omp_set_lock(&bufferLock);
	std::ptrdiff_t n = pptr() - pbase();
  if (omp_get_thread_num() != 0) {
    std::cerr << "<start cerr> sync called from non-root thread (" << omp_get_thread_num() << "): ";
    std::cerr.write(pbase(), n);
    std::cerr << "<end cerr>" << std::endl << std::flush;

//    void* callstack[128];
//    int i, frames = backtrace(callstack, 128);
//    char** strs = backtrace_symbols(callstack, frames);
//    for (i = 0; i < frames; ++i) {
//      printf("%s\n", strs[i]);
//    }
//    free(strs);
  } else {
    rStream.write(pbase(), n);
    rStream.flush();
    if ((rStream.rdstate() & (std::ostream::failbit | std::ostream::badbit)) != 0) {
      omp_unset_lock(&bufferLock);
      return -1;
    }
  }

	pbump(-n);	
  omp_unset_lock(&bufferLock);
	return 0;
}
