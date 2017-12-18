#include <algorithm>
#include <vector>

#include <Rcpp.h>

using namespace Rcpp;
using namespace std;

// [[Rcpp::export]]
vector<int> danceSeq(vector<int> positions, vector<int> moves, int amount){
  
  vector<int> r;
  vector<vector<int> > storage;
  for(int a = 0; a < amount; ++a){
    for(unsigned i = 2; i <= moves.size(); i += 3){

      switch(moves[i-2]){
      case 0 :  
        for(int x = 0; x < moves[i-1]; ++x){
          int l = positions.back();
          positions.pop_back();
          positions.insert(positions.begin(), l);
        };
        break;
      case 1 : 
        iter_swap(positions.begin() + moves[i-1], positions.begin() +moves[i]);
        break;
      case 2: 
        iter_swap(find(positions.begin(), positions.end(), moves[i-1]), find(positions.begin(), positions.end(), moves[i]));
        break;
      }
    }
    if(storage.empty() || find(storage.begin(), storage.end(), positions) == storage.end())
      storage.push_back(positions);
    else{
      positions = storage[amount % a - 1];
      break;
    }
  }
  return positions;
}
