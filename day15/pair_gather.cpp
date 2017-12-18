#include <bitset>
#include <vector>

#include <Rcpp.h>

#define GEN_A 16807
#define GEN_B 48271
#define MAX_INT 2147483647

using namespace Rcpp;
using namespace std;


bool compareSets(bitset<16> a, bitset<16> b){
  bool var = true;

  for(int i = 0; i <= 15; ++i){
    if(a[i] != b[i]){
      var = false;
      // Stop the compare if any differences were found
      break;
    }
  }
  return var;
}


int judgeFunction(long long int a, long long int b){
  
  int value = 0;
  
  // take the first 16 bits
  bitset<16> setA(a);
  bitset<16> setB(b);
  
  if(compareSets(setA,setB)){
    ++value;
  }
  
  return value;
}


// [[Rcpp::export]]
int firstSampling(unsigned startA, unsigned startB, int amount){
  
  int count = 0;
  
  long long int a = (long long int) startA;
  long long int b = (long long int) startB;
  
  for(int i = 0; i < amount; ++i){
    
    a = (a * GEN_A) % MAX_INT;
    b = (b * GEN_B) % MAX_INT;

    count += judgeFunction(a, b);
  }
  return count;
}

// [[Rcpp::export]]
int secondSampling(unsigned startA, unsigned startB, int amount){
  
  
  long long int a = (long long int) startA;
  long long int b = (long long int) startB;
  
  vector<long long int> aData(amount);
  vector<long long int> bData(amount);
  
  int aCount = 0;
  int bCount = 0;
  
  double counter = 0;
  // Construct the pairs
  while(counter < amount){
    if(aCount < amount){
      a = (a * GEN_A) % MAX_INT;
      
      if(a % 4 == 0){
        aData[aCount] = a;
        counter += 0.5;
        ++aCount;
      }
    }
    if(bCount < amount){
      b = (b * GEN_B) % MAX_INT;
      
      if(b % 8 == 0){
        bData[bCount] = b;
        counter += 0.5;
        ++bCount;
      }
    }
  }
  int count = 0;
  for(int i = 0; i < amount; ++i){
    count += judgeFunction(aData[i], bData[i]); 
  }
  
  return count;
}