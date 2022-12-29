#include "ns.hpp"
#include "mtlpp.hpp" 

namespace mtlpp {
    class LinkedFunctions : public ns::Object
    {
        public:
            LinkedFunctions();

            ns::Array<mtlpp::Function>*   GetFunctions();
            void                          SetFunctions(const ns::Array<mtlpp::Function>* functions);

            ns::Array<mtlpp::Function>*   GetBinaryFunctions();
            void                          SetBinaryFunctions(const ns:Array<mtlpp::Function>* binaryFunctions);
            
            ns::Array<mtlpp::Function>*   GetPrivateFunctions();
            void                          SetPrivateFunctions(const ns::Array<mtlpp::Function>* privateFunctions);

            ns::Array<ns::Dictionary<ns::String*,ns::Array<mtlpp::Function>>* GetGroups();
            void SetGroups(const ns::Array<ns::Dictionary<ns::String*,ns::Array<mtlpp::Function>>* groups);
    };
}
