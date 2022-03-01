/*
 * Copyright 2016-2017 Nikolay Aleksiev. All rights reserved.
 * License: https://github.com/naleksiev/mtlpp/blob/master/LICENSE
 */

#pragma once

#include "defines.hpp"

namespace ns
{
        struct Handle
        {
            const void* ptr;
        };

        class Object
        {
        public:
            inline const void* GetPtr() const { return m_ptr; }

            inline operator bool() const { return m_ptr != nullptr; }

        protected:
            Object();
            Object(const Handle& handle);
            Object(const Object& rhs);
    #if MTLPP_CONFIG_RVALUE_REFERENCES
            Object(Object&& rhs);
    #endif
            virtual ~Object();

            Object& operator=(const Object& rhs);
    #if MTLPP_CONFIG_RVALUE_REFERENCES
            Object& operator=(Object&& rhs);
    #endif

            inline void Validate() const
            {
    #if MTLPP_CONFIG_VALIDATE
                assert(m_ptr);
    #endif
            }

            const void* m_ptr = nullptr;
        };

        struct Range
        {
            inline Range(uint32_t location, uint32_t length) :
                Location(location),
                Length(length)
            { }

            uint32_t Location;
            uint32_t Length;
        };

        class ArrayBase : public Object
        {
        public:
            ArrayBase() { }
            ArrayBase(const Handle& handle) : Object(handle) { }

            uint32_t GetSize() const;

        protected:
            void* GetItem(uint32_t index) const;
        };

        template<typename T>
        class Array : public ArrayBase
        {
        public:
            Array() { }
            Array(const Handle& handle) : ArrayBase(handle) { }

            const T operator[](uint32_t index) const
            {
                return Handle{ GetItem(index) };
            }

            T operator[](uint32_t index)
            {
                return Handle{ GetItem(index) };
            }
        };

        class DictionaryBase : public Object
        {
        public:
            DictionaryBase() { }
            DictionaryBase(const Handle& handle) : Object(handle) { }

        protected:

        };

        template<typename KeyT, typename ValueT>
        class Dictionary : public DictionaryBase
        {
        public:
            Dictionary() { }
            Dictionary(const Handle& handle) : DictionaryBase(handle) { }
        };

        class String : public Object
        {
        public:
            String() { }
            String(const Handle& handle) : Object(handle) { }
            String(const char* cstr);

            const char* GetCStr() const;
            uint32_t    GetLength() const;
        };

        class Error : public Object
        {
        public:
            Error();
            Error(const Handle& handle) : Object(handle) { }

            String   GetDomain() const;
            uint32_t GetCode() const;
            //@property (readonly, copy) NSDictionary *userInfo;
            String   GetLocalizedDescription() const;
            String   GetLocalizedFailureReason() const;
            String   GetLocalizedRecoverySuggestion() const;
            String   GetLocalizedRecoveryOptions() const;
            //@property (nullable, readonly, strong) id recoveryAttempter;
            String   GetHelpAnchor() const;
        };

        class URL : public Object
        {
        public:

            URL();
            URL(const String* pString);
            URL(const String* pPath);
            const char* fileSystemRepresentation() const;
        };

        class Bundle : public Object
        {
            // private:
            //     _NS_CONST(NotificationName, BundleDidLoadNotification);
            //     _NS_CONST(NotificationName, BundleResourceRequestLowDiskSpaceNotification);

            public:
                static Bundle*    mainBundle();
                Bundle(const class String* pPath);
                Bundle(const class URL* pURL);
                Bundle(const ns::Handle& handle) : ns::Object(handle) { }

                Bundle*           init(const class String* pPath);
                Bundle*           init(const class URL* pURL);

                Array<Bundle>*      allBundles() const;
                //Array*              allFrameworks() const; //revisit

                bool              load();
                bool              unload();

                bool              isLoaded() const;

                bool              preflightAndReturnError(class Error** pError) const;
                bool              loadAndReturnError(class Error** pError);

                class URL*        bundleURL() const;
                class URL*        resourceURL() const;
                class URL*        executableURL() const;
                class URL*        URLForAuxiliaryExecutable(const class String* pExecutableName) const;

                class URL*        privateFrameworksURL() const;
                class URL*        sharedFrameworksURL() const;
                class URL*        sharedSupportURL() const;
                class URL*        builtInPlugInsURL() const;
                class URL*        appStoreReceiptURL() const;

                class String*     bundlePath() const;
                class String*     resourcePath() const;
                class String*     executablePath() const;
                class String*     pathForAuxiliaryExecutable(const class String* pExecutableName) const;

                class String*     privateFrameworksPath() const;
                class String*     sharedFrameworksPath() const;
                class String*     sharedSupportPath() const;
                class String*     builtInPlugInsPath() const;

                class String*     bundleIdentifier() const;
                class Dictionary* infoDictionary() const;
                class Dictionary* localizedInfoDictionary() const;
                class Object*     objectForInfoDictionaryKey(const String* pKey);

                class String* localizedString(const  String* pKey, const String* pValue = nullptr, const String* pTableName = nullptr) const;
                class String* LocalizedString(const String* pKey, const String*);
                class String* LocalizedStringFromTable(const String* pKey, const String* pTbl, const String*);
                class String* LocalizedStringFromTableInBundle(const String* pKey, const String* pTbl, const Bundle* pBdle, const String*);
                class String* LocalizedStringWithDefaultValue(const String* pKey, const String* pTbl, const Bundle* pBdle, const String* pVal, const String*);
        };
    
        class Data : public Object
        {
        public:
            void*    SetMutableBytes();
            uint64_t GetLength();
        };
    }
}

