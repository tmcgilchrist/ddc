
#ifndef _DDC_Death
#define _DDC_Death

#include "Types.h"

// -- Death
void	_deathCase 
		(const Char8* moduleName, Int32 line, Int32 column) 
		__attribute__((noreturn));

void	_deathEval (void)
		__attribute__((noreturn));

void	_panicOutOfHeap	 
		(UInt allocCount, UInt64 heapSize)
		__attribute__((noreturn));

void	_panicOutOfSlots (void)
		__attribute__((noreturn));
		
void	_panicApply (void)
		__attribute__((noreturn));
		
		
void	_panicSlotUnderflow (void)
		__attribute__((noreturn));


void	_panicCorruption (void)
		__attribute__((noreturn));


#include "Error.ci"

#endif
