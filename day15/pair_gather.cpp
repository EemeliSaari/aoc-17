#include <bitset>
#include <iostream>
#include <vector>
#include <cstdio>
#include <ctime>

#include <Rcpp.h>

#define GEN_A 16807
#define GEN_B 48271
#define MAX_INT 2147483647

using namespace Rcpp;
using namespace std;


bool compareSets(bitset<32> a, bitset<32> b){
  bool var = true;
  for(int i = 16; i <= 32; ++i){
    if(a[i] != b[i]){
      var = false;
      break;
    }
  }
  return var;
}


int judgeFunction(unsigned a, unsigned b){
  
  int value = 0;
  
  bitset<32> setA(a);
  bitset<32> setB(b);
  
  if(compareSets(setA,setB)){
    ++value;
  }
  
  return value;
}


// [[Rcpp::export]]
vector<double> firstSampling(unsigned startA, unsigned startB, int amount){
  
  vector<double> timeSamples(amount);
  int count = 0;
  
  clock_t start = clock();
  
  unsigned long long a = (unsigned long long) startA;
  unsigned long long b = (unsigned long long) startB;
  
  for(int i = 0; i <= amount; ++i){
    
    a = (a * GEN_A) % MAX_INT;
    b = (b * GEN_B) % MAX_INT;
    
    cout << a << " vs " << b << endl;
    
    count += judgeFunction(a, b);
    
    timeSamples[i] = (clock() - start) / (double) CLOCKS_PER_SEC;
  }
  
  cout << "First sample: " << count << endl;
  return timeSamples;
}

// [[Rcpp::export]]
NumericVector secondSampling(unsigned startA, unsigned startB, int amount){
  
  NumericVector timeSamples(amount);
  int count = 0;
  
  for(int i = 0; i < amount; ++i){
    clock_t start = clock();
    
    timeSamples[i] = (clock() - start) / (double) CLOCKS_PER_SEC;
  }
  
  cout << "Second sample: " << count << endl;
  return timeSamples;
}