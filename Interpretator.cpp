#include <fstream>
#include <iostream>
#include <memory>
#include <string>
#include <unordered_map>
#include <map>
#include <set>
#include <stack>
#include <vector>


using namespace std;

// --- подготовка ПОЛИЗ и его выполнение
namespace execution {

    using VariableName = string;
    using OperationIndex = size_t;

    enum ValueType { Str, Int, Logic };

    const string& ToString(ValueType type) {
        static const unordered_map<ValueType, string> kResults{
          {Str, "string"}, {Int, "int"}, {Logic, "bool"},
        };
        return kResults.at(type);
    }

    // полиморфное значение
    class PValue {
    public:
        PValue(const char* str) : type_(Str), str_(str) {}
        PValue(const string& str) : type_(Str), str_(str) {}
        PValue(int integral) : type_(Int), integral_(integral) {}
        PValue(bool logic) : type_(Logic), logic_(logic) {}

        ValueType GetValueType() { return type_; }
        string& Get_str() { CheckIs(Str);  return str_; } // возвращает строковое значение
        int Get_int() { CheckIs(Int);   return integral_; } // возвращает целое значение
        bool Get_bool() { CheckIs(Logic);  return logic_; } // возвращает логическое значение

        operator string() const { CheckIs(Str); return str_; } // оператор преобразования PValue к string
        operator int() const { CheckIs(Int); return integral_; } // оператор преобразования PValue к int
        operator bool() const { CheckIs(Logic); return logic_; } // оператор преобразования PValue к bool

    private:
        ValueType type_; // тип переменной
        string str_; // строковое значение
        int integral_ = 0; // целое значение
        bool logic_ = false; // логическое значение
 
        // проверка типа с генерацией ошибки
        void CheckIs(ValueType type) const {
            if (type != type_) {
                // todo - номер строки - done
                throw std::runtime_error("Oh, no! - Type mismatch " + ToString(type) + " is not actual " + ToString(type_));
            }
        }
    };

    // полиморфная переменная
    class PVariable {
    public:
        PVariable(VariableName name, ValueType type) : name_(name), type_(type) {}
        PVariable& operator = (PValue pv) {
            type_ = pv.GetValueType();
            switch (type_) {

                //case Int: { integral_ = (int)pv; break; }
            case Int: { integral_ = pv.Get_int(); break; }
            case Str: { str_ = pv.Get_str(); break; }
            case Logic: { logic_ = pv.Get_bool(); break; } // iLineForError_ = pv.GetLineForError(); 
            }
            return *this;
        }

        bool IsAssign() { return assign_; }  // проверка инициализирована переменная или нет
        void Set(const string& str) { CheckIs(Str);  str_ = str; assign_ = true; } // записывает строковое значение
        void Set(int integral) { CheckIs(Int);  integral_ = integral;  assign_ = true; } // записывает целое значение
        void Set(bool logic) { CheckIs(Logic);  logic = logic_; assign_ = true; } // записывает логическое значение

        ValueType GetValueType() { return type_; }
        void SetAssign() { assign_ = true; } // устанавливает статус assign_ в true
        string& Get_str() { CheckIs(Str);  CheckAssign(); return str_; } // возвращает строковое значение
        int Get_int() { CheckIs(Int);  CheckAssign(); return integral_; } // возвращает целое значение
        bool Get_bool() { CheckIs(Logic);  CheckAssign(); return logic_; } // возвращает логическое значение

        operator string() const { CheckIs(Str); return str_; } // оператор преобразования PVariable к string
        operator int() const { CheckIs(Int); return integral_; } // оператор преобразования PVariable к int
        operator bool() const { CheckIs(Logic); return logic_; } // оператор преобразования PVariable к bool

    protected:
        VariableName name_; // имя переменной
        ValueType type_; // тип переменной
        string str_; // строковое значение
        int integral_ = 0; // целое значение
        bool logic_ = false; // логическое значение
        bool assign_ = false; // true - переменная инициализирована, 0 - в противном случае

        // проверка инициализации переменной
        void CheckAssign() const {
            if (!assign_) {
                throw std::runtime_error("Oh, no! -  Variable " + name_ + " is not assign ");
            }
        }
        // проверка типа с генерацией ошибки
        void CheckIs(ValueType type) const {
            if (type != type_) {
                throw std::runtime_error("Oh, no! - Type mismatch " + ToString(type) + " is not actual " + ToString(type_));
            }
        }
    };

    // контекст
    struct Context {
        size_t operation_index = 0; // индекс
        stack<PValue> stack; // стек значений PValue, обрабатываемых программой
        unordered_map<VariableName, unique_ptr<PVariable>> variables; // карта указателей переменных PVariable, используемых в программе
        int iCurLine = 0; // текущая строка программы для генерации ошибки на этапе выполнения
    };

    // базовая структура операций
    struct Operation {
        virtual ~Operation() {}
        virtual void Do(Context& context) const = 0;
    };

    // значение как операнд
    struct ValueOperation : Operation {
        ValueOperation(PValue value, int iCurLine) : value_(value), iCurLine_(iCurLine) {}

        void Do(Context& context) const final {
            context.iCurLine = iCurLine_;  context.stack.push(value_);
        }

    private:
        const PValue value_; // значение
        const int iCurLine_;
    };

    // добавить новую переменную (описание)
    struct AddVariableOperation : Operation {
        AddVariableOperation(const VariableName& name, const ValueType type, int iCurLine) : name_(name), type_(type), iCurLine_(iCurLine) {}

        void Do(Context& context) const final {
            context.iCurLine = iCurLine_; context.variables[name_].reset(new PVariable(name_, type_)); // добавляем переменную в карту variables
        }
    private:
        const VariableName name_;
        const ValueType type_;
        const int iCurLine_;
    };

    // переменная как операнд
    struct VariableOperation : Operation {
        VariableOperation(const VariableName& name, int iCurLine) : name_(name), iCurLine_(iCurLine) {}

        void Do(Context& context) const final {
            // метод .at ищет значение по ключу и если его нет бросает исключение
            // метод .get возвращает указатель, которым владеет unique_ptr
            //context.stack.emplace(
            context.iCurLine = iCurLine_;
            ValueType vt = context.variables.at(name_).get()->GetValueType(); // берем тип переменной
           // в зависимости от типа переменной создаем нужное значение PValue и кладем его в стек
            switch (vt) {
            case Int: {
                PValue pv(context.variables.at(name_).get()->Get_int());
                context.stack.push(pv);
                break;
            }
            case Str: {
                PValue pv(context.variables.at(name_).get()->Get_str());
                context.stack.push(pv);
                break;
            }
            case Logic: {
                PValue pv(context.variables.at(name_).get()->Get_bool());
                context.stack.push(pv);
                break;
            }
            }
        }

    private:
        const VariableName name_;
        const int iCurLine_;
    };

    // оператор присваивания
    struct AssignOperation : Operation {
        AssignOperation(const VariableName& name, int iCurLine) : name_(name), iCurLine_(iCurLine) {}

        void Do(Context& context) const final {
            context.iCurLine = iCurLine_;
            // извлекаем значение из стека
            PValue op = context.stack.top();
            context.stack.pop();
            // присваиваем значение переменной
            *context.variables.at(name_).get() = op;
            context.variables.at(name_).get()->SetAssign();
        }
    private:
        const VariableName name_;
        const int iCurLine_;
    };

    // оператор присваивания
    struct ReadOperation : Operation {
        ReadOperation(const VariableName& name, int iCurLine) : name_(name), iCurLine_(iCurLine) {}

        void Do(Context& context) const final {
            context.iCurLine = iCurLine_;
            string sTmp;
            // вводим значение и заносим его в переменную
            ValueType vt = context.variables.at(name_).get()->GetValueType();

            switch (vt) {
            case Int: { getline(cin, sTmp); int iTmp; iTmp = stoi(sTmp); context.variables.at(name_).get()->Set(iTmp); break; }
            case Str: { getline(cin, sTmp); context.variables.at(name_).get()->Set(sTmp); break; }
            case Logic: { getline(cin, sTmp); bool bTmp; bTmp = (bool)stoi(sTmp); context.variables.at(name_).get()->Set(bTmp); break; }
            }
        }
    private:
        const VariableName name_;
        const int iCurLine_;
    };

    // безусловный переход
    struct GoOperation : Operation {
        GoOperation(OperationIndex index, int iCurLine) : index_(index), iCurLine_(iCurLine) {}

        void Do(Context& context) const override {
            context.iCurLine = iCurLine_;
            context.operation_index = index_;
        }

    private:
        const OperationIndex index_;
        const int iCurLine_;
    };

    // Операция - условный переход по лжи
    struct IfOperation : GoOperation {
        IfOperation(OperationIndex index, int iCurLine) : GoOperation(index, iCurLine) {}

        void Do(Context& context) const final {
            // извлекаем операнд
            PValue value = context.stack.top();
            context.stack.pop();

            // выполняем переход по лжи
            if (!value) {
                GoOperation::Do(context);
            }
        }
    };

    using Operations = vector<shared_ptr<Operation>>;

    // унарный минус
    template <typename T>
    struct UnaryMinusOperation : Operation {
        UnaryMinusOperation(int iCurLine) : iCurLine_(iCurLine) {}
        void Do(Context& context) const final;
    private:
        const int iCurLine_;
    };
    template <typename T>
    void UnaryMinusOperation<T>::Do(Context& context) const {
        context.iCurLine = iCurLine_;
        const PValue new_value(-static_cast<T>(context.stack.top()));
        context.stack.pop();
        context.stack.push(new_value);
    }

    // операция печати - Write
    template <typename T>
    struct PrintOperation : Operation {
        PrintOperation(int iCurLine) : iCurLine_(iCurLine) {}
        void Do(Context& context) const final {
            context.iCurLine = iCurLine_;
            const PValue op = context.stack.top();
            context.stack.pop();
            cout << static_cast<T>(op);
        }
    private:
        const int iCurLine_;
    };

    // операция печати - Writeln
    template <typename T>
    struct PrintlnOperation : Operation {
        PrintlnOperation(int iCurLine) : iCurLine_(iCurLine) {}
        void Do(Context& context) const final {
            context.iCurLine = iCurLine_;
            const PValue op = context.stack.top();
            context.stack.pop();
            cout << static_cast<T>(op) << endl;
        }
    private:
        const int iCurLine_;
    };


    // базовая структура математических операций
    struct MathOperation : Operation {
        MathOperation(int iCurLine) : iCurLine_(iCurLine) {}
        void Do(Context& context) const final {
            context.iCurLine = iCurLine_;
            // извлекаем второй операнд
            PValue op2 = context.stack.top();
            context.stack.pop();
            // извлекаем первый операнд
            PValue op1 = context.stack.top();
            context.stack.pop();
            // записываем результат операции
            context.stack.push(DoMath(op1, op2));
        }

        virtual PValue DoMath(PValue op1, PValue op2) const = 0;
    private:
        const int iCurLine_;
    };

    // сложение
    template <typename T>
    struct PlusOperation : MathOperation {
        PlusOperation(int iCurLine) : MathOperation(iCurLine) {}
        PValue DoMath(PValue op1, PValue op2) const final {
            return PValue(static_cast<T>(op1) + static_cast<T>(op2));
        }
    };

    // вычитание
    template <typename T>
    struct MinusOperation : MathOperation {
        MinusOperation(int iCurLine) : MathOperation(iCurLine) {}
        PValue DoMath(PValue op1, PValue op2) const final {
            return PValue(static_cast<T>(op1) - static_cast<T>(op2));
        }
    };

    // умножение
    template <typename T>
    struct MulOperation : MathOperation {
        MulOperation(int iCurLine) : MathOperation(iCurLine) {}
        PValue DoMath(PValue op1, PValue op2) const final {
            return PValue(static_cast<T>(op1) * static_cast<T>(op2));
        }
    };

    // деление
    template <typename T>
    struct DivOperation : MathOperation {
        DivOperation(int iCurLine) : MathOperation(iCurLine) {}
        PValue DoMath(PValue op1, PValue op2) const final {
            return PValue(static_cast<T>(op1) / static_cast<T>(op2));
        }
    };


    // ==
    template <typename T>
    struct EqualOperation : MathOperation {
        EqualOperation(int iCurLine) : MathOperation(iCurLine) {}
        PValue DoMath(PValue op1, PValue op2) const final {
            return PValue(static_cast<T>(op1) == static_cast<T>(op2));
        }
    };

    // !=
    template <typename T>
    struct NotEqualOperation : MathOperation {
        NotEqualOperation(int iCurLine) : MathOperation(iCurLine) {}
        PValue DoMath(PValue op1, PValue op2) const final {
            return PValue(static_cast<T>(op1) != static_cast<T>(op2));
        }
    };


    // <
    template <typename T>
    struct LessOperation : MathOperation {
        LessOperation(int iCurLine) : MathOperation(iCurLine) {}
        PValue DoMath(PValue op1, PValue op2) const final {
            return PValue(static_cast<T>(op1) < static_cast<T>(op2));
        }
    };

    // <=
    template <typename T>
    struct LessOrEqualOperation : MathOperation {
        LessOrEqualOperation(int iCurLine) : MathOperation(iCurLine) {}
        PValue DoMath(PValue op1, PValue op2) const final {
            return PValue(static_cast<T>(op1) <= static_cast<T>(op2));
        }
    };

    // >
    template <typename T>
    struct MoreOperation : MathOperation {
        MoreOperation(int iCurLine) : MathOperation(iCurLine) {}
        PValue DoMath(PValue op1, PValue op2) const final {
            return PValue(static_cast<T>(op1) > static_cast<T>(op2));
        }
    };

    // >=
    template <typename T>
    struct MoreOrEqualOperation : MathOperation {
        MoreOrEqualOperation(int iCurLine) : MathOperation(iCurLine) {}
        PValue DoMath(PValue op1, PValue op2) const final {
            return PValue(static_cast<T>(op1) >= static_cast<T>(op2));
        }
    };

    // Not
    struct NotOperation : Operation {
        NotOperation(int iCurLine) : iCurLine_(iCurLine) {}
        void Do(Context& context) const final;
    private:
        const int iCurLine_;
    };
    void NotOperation::Do(Context& context) const {
        context.iCurLine = iCurLine_;
        const PValue new_value(!(context.stack.top()));
        context.stack.pop();
        context.stack.push(new_value);
    }

    // And
    struct AndOperation : Operation {
        AndOperation(int iCurLine) : iCurLine_(iCurLine) {}
        void Do(Context& context) const final;
    private:
        const int iCurLine_;
    };
    void AndOperation::Do(Context& context) const {
        context.iCurLine = iCurLine_;
        // извлекаем второй операнд
        PValue op2 = context.stack.top();
        context.stack.pop();
        // извлекаем первый операнд
        PValue op1 = context.stack.top();
        context.stack.pop();
        // записываем результат операции
        context.stack.push(op1 && op2);
    }

    // Or
    struct OrOperation : Operation {
        OrOperation(int iCurLine) : iCurLine_(iCurLine) {}
        void Do(Context& context) const final;
    private:
        const int iCurLine_;
    };
    void OrOperation::Do(Context& context) const {
        context.iCurLine = iCurLine_;
        // извлекаем второй операнд
        PValue op2 = context.stack.top();
        context.stack.pop();
        // извлекаем первый операнд
        PValue op1 = context.stack.top();
        context.stack.pop();
        // записываем результат операции
        context.stack.push(op1 || op2);
    }

    enum OperationType { UnaryMinus, Plus, Minus, Mul, Div, Equal, NotEqual, Less, LessOrEqual, More, MoreOrEqual, Print, Println };

    // создание операции, возвращает указатель на операцию
    template <typename T>
    Operation* MakeOp(int i) {
        return new T(i);
    }

    using OperationBuilder = Operation * (*)(int); // указатель на функцию-обработчик операции

    using KeyOperation = tuple<OperationType, ValueType>;
    
    static const map<KeyOperation, OperationBuilder> mOperations{
    {{UnaryMinus, Int}, &MakeOp <UnaryMinusOperation<int>>},
    {{Plus, Int}, &MakeOp <PlusOperation<int>>},
   {{Plus, Str}, &MakeOp <PlusOperation<string>>},
    {{Minus, Int}, &MakeOp <MinusOperation<int>>},
    {{Mul, Int}, &MakeOp < MulOperation<int>>},
    {{Div, Int}, &MakeOp < DivOperation<int>>},
    {{Equal, Int}, &MakeOp < EqualOperation<int>>},
    {{Equal, Str}, &MakeOp < EqualOperation<string>>},
    {{Equal, Logic}, &MakeOp < EqualOperation<bool>>},
    {{NotEqual, Int}, &MakeOp < NotEqualOperation<int>>},
    {{NotEqual, Str}, &MakeOp < NotEqualOperation<string>>},
    {{NotEqual, Logic}, &MakeOp < NotEqualOperation<bool>>},
    {{Less, Int}, &MakeOp < LessOperation<int>>},
    {{Less, Str}, &MakeOp < LessOperation<string>>},
    {{Less, Logic}, &MakeOp < LessOperation<bool>>},
    {{LessOrEqual, Int}, &MakeOp < LessOrEqualOperation<int>>},
    {{LessOrEqual, Str}, &MakeOp < LessOrEqualOperation<string>>},
    {{LessOrEqual, Logic}, &MakeOp < LessOrEqualOperation<bool>>},
    {{More, Int}, &MakeOp < MoreOperation<int>>},
    {{More, Str}, &MakeOp < MoreOperation<string>>},
    {{More, Logic}, &MakeOp < MoreOperation<bool>>},
    {{MoreOrEqual, Int}, &MakeOp < MoreOrEqualOperation<int>>},
    {{MoreOrEqual, Str}, &MakeOp < MoreOrEqualOperation<string>>},
    {{MoreOrEqual, Logic}, &MakeOp < MoreOrEqualOperation<bool>>},
    {{Print, Int}, &MakeOp <PrintOperation<int>>},
    {{Print, Str}, &MakeOp <PrintOperation<string>>},
    {{Print, Logic}, &MakeOp <PrintOperation<bool>>},
    {{Println, Int}, &MakeOp <PrintlnOperation<int>>},
    {{Println, Str}, &MakeOp <PrintlnOperation<string>>},
    {{Println, Logic}, &MakeOp <PrintlnOperation<bool>>} 
    };

    Operations operations;
    Context context;

} // end of namespace execution

// --- Лексический анализ
namespace TokenNS {

    // Тип лексемы
    enum LexemType {
        LexemLeftSquare,  // левая квадратная скобка
        LexemRightSquare, // правая квадратная скобка
        LexemBooleanConst, // логическая константа
        LexemIntegerConst, // целочисленная константа
        LexemStringConst, // строковая константа
        LexemProgram, // ключевое слово Program
        LexemBoolean, // ключевое слово Boolean
        LexemInteger, // ключевое слово Integer
        LexemString, // ключевое слово String
        LexemBegin, // ключевое слово Begin
        LexemWrite,
        LexemWriteln,
        LexemRead,
        LexemWhile,
        LexemDo,
        LexemEnd,
        LexemVar,
        LexemAnd,
        LexemElse,
        LexemIf,
        LexemNot,
        LexemOr,
        LexemThen,
        LexemSemicolon, // разделитель точка с запятой
        LexemColon, // разделитель двоеточие
        LexemAssign, // разделитель присваивание
        LexemPoint, // разделитель точки
        LexemComma, // разделитель запятой
        LexemLeftBracket,
        LexemRightBracket,
        LexemSum,
        LexemMinus,
        LexemMult,
        LexemDiv,
        LexemNEQ,
        LexemEQ,
        LexemLess,
        LexemMore,
        LexemLessEQ,
        LexemMoreEQ,
        LexemIdentifier, // идентификатор
    };

    // Лексема
    struct Lexem {
        LexemType type;
        string value;
        // bool boolean_value;
        // числовую константу удобнее хранить в виде строки для контроля переполнения
            // (более подробно поясню в субботу)
    };

    class Token {

        friend ostream& operator<<(ostream&, const Lexem&);
    public:
        Token(istream& input);

        using Char = istream::int_type;
        using bState = bool (Token::*)(Char c);

        bool HasLexem();
        Lexem GetLexem();
        const Lexem& LinkLexem() const;
        inline int GetCurLine() { return iCurLineNum; } // возвращает  номер текущей строки

    private:
        // todo - private - done
        static const unordered_map<LexemType, string> mTokenForPrint;
        static const unordered_map<Char, LexemType> mSeparators;
        static const unordered_map<string, LexemType> mKeyWords;
        static const set<Char> mOtherSymbol;
        
        int iCurLineNum; // номер текущей строки программы


        istream& curInput; // входящий для обработки поток
        bState curState; // указатель на текущую функцию обработчика состояния
        bool curHasLexem; // статус - сформирована ли лексема
        Lexem curLexem; // текущая лексема

        bool IsChar(Char c); // проверка является ли символ буквой
        bool IsDigit(Char c); // проверка является ли символ цифрой от 0 до 9
        bool IsDigitNZ(Char c); // проверка является ли символ цифрой от 1 до 9
        bool IsOtherSymbol(Char c); // проверка входит ли символ в множество mOtherSymbol
        bool IsLegalChar(Char c); // // проверка входит ли символ в множество разрешенных символов

        bool bInitial(Char c); // функция обработки состояния Initial
        bool bNumber(Char c); // функция обработки состояния Number
        bool bAssign(Char c); // функция обработки состояния  Assign
        bool bMore(Char c); // функция обработки состояния  More
        bool bNEQLess(Char c); // функция обработки состояния  NEQLess
        bool bWord(Char c); // функция обработки состояния  Word
        bool bStr(Char c); // функция обработки состояния  Str
        bool bStrScreen(Char c); // функция обработки состояния  StrScreen
        bool bComment(Char c); // функция обработки состояния  Comment
    };

    Token::Token(istream& input) :
        iCurLineNum(1),
        curInput(input), // задаем начальные значения: входной поток - input,
        curState(nullptr), // текущее состояние - нулевой указатель
        curHasLexem(false) // нет лексемы
    {}

    // проверка является ли символ буквой
    inline bool Token::IsChar(Char c) {
        return ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z'));
    }

    // проверка является ли символ цифрой от 0 до 9
    inline bool Token::IsDigit(Char c) {
        return (c >= '0' && c <= '9');
    }

    // проверка является ли символ цифрой от 1 до 9
    inline bool Token::IsDigitNZ(Char c) {
        return (c >= '1' && c <= '9');
    }

    // проверка входит ли символ в множество mOtherSymbol
    inline bool Token::IsOtherSymbol(Char c) {
        return (mOtherSymbol.find(c) != mOtherSymbol.end());
    }

    // проверка входит ли символ в множество разрешенных символов
    inline bool Token::IsLegalChar(Char c) {
            return (IsChar(c) || IsDigit(c) || IsOtherSymbol(c) || c == '\''|| c == '{' || c == '}' || c == '.' || c == 13 || c == 10 || c == EOF);
    }

    // карта лексем для печати
    const unordered_map<LexemType, string>Token::mTokenForPrint{
        {LexemType::LexemLeftSquare, "LexemLeftSquare" },
        {LexemType::LexemRightSquare, "LexemRightSquare" },
        {LexemType::LexemBooleanConst, "LexemBooleanConst" },
        {LexemType::LexemIntegerConst, "LexemIntegerConst" },
        {LexemType::LexemStringConst, "LexemStringConst" },
        {LexemType::LexemIdentifier, "LexemIdentifier" },
        {LexemType::LexemAnd, "LexemAnd" },
        {LexemType::LexemWrite, "LexemWrite" },
        {LexemType::LexemWriteln, "LexemWriteln" },
        {LexemType::LexemRead, "LexemRead" },
        {LexemType::LexemWhile, "LexemWhile" },
        {LexemType::LexemDo, "LexemDo" },
        {LexemType::LexemAssign, "LexemAssign" },
        {LexemType::LexemBegin, "LexemBegin" },
        {LexemType::LexemBoolean, "LexemBoolean" },
        {LexemType::LexemColon, "LexemColon" },
        {LexemType::LexemComma, "LexemComma" },
        {LexemType::LexemElse, "LexemElse" },
        {LexemType::LexemEnd, "LexemEnd" },
        {LexemType::LexemEQ, "LexemEQ" },
        {LexemType::LexemIf, "LexemIf" },
        {LexemType::LexemInteger, "LexemInteger" },
        {LexemType::LexemLeftBracket, "LexemLeftBracket" },
        {LexemType::LexemLess, "LexemLess" },
        {LexemType::LexemLessEQ, "LexemLessEQ" },
        {LexemType::LexemMinus, "LexemMinus" },
        {LexemType::LexemMore, "LexemMore" },
        {LexemType::LexemMoreEQ, "LexemMoreEQ" },
        {LexemType::LexemMult, "LexemMult" },
        {LexemType::LexemDiv, "LexemDiv" },
        {LexemType::LexemNEQ, "LexemNEQ" },
        {LexemType::LexemNot,"LexemNot"},
        {LexemType::LexemOr, "LexemOr"},
        {LexemType::LexemPoint,"LexemPoint"},
        {LexemType::LexemProgram,"LexemProgram"},
        {LexemType::LexemRightBracket,"LexemRightBracket"},
        {LexemType::LexemSemicolon,"LexemSemicolon"},
        {LexemType::LexemString,"LexemString"},
        {LexemType::LexemSum,"LexemSum"},
        {LexemType::LexemThen,"LexemThen"},
        {LexemType::LexemVar,"LexemVar"},
    };

    // карта ключевых слов
    const unordered_map<string, LexemType>Token::mKeyWords{
        {"PROGRAM", LexemType::LexemProgram},
        {"VAR", LexemType::LexemVar},
        {"BEGIN", LexemType::LexemBegin},
        {"END", LexemType::LexemEnd},
        {"BOOLEAN", LexemType::LexemBoolean},
        {"INTEGER", LexemType::LexemInteger},
        {"STRING", LexemType::LexemString},
        {"WRITE", LexemType::LexemWrite},
        {"WRITELN", LexemType::LexemWriteln},
        {"READ", LexemType::LexemRead},
        {"WHILE", LexemType::LexemWhile},
        {"DO", LexemType::LexemDo,},
        {"IF", LexemType::LexemIf},
        {"THEN", LexemType::LexemThen},
        {"ELSE", LexemType::LexemElse},
        {"TRUE", LexemType::LexemBooleanConst},
        {"FALSE", LexemType::LexemBooleanConst},
        {"AND", LexemType::LexemAnd},
        {"OR", LexemType::LexemOr},
        {"NOT", LexemType::LexemNot}
    };

    // карта разделителей
    const unordered_map<Token::Char, LexemType> Token::mSeparators{
        {'[', LexemType::LexemLeftSquare},
        {']', LexemType::LexemRightSquare },
        {'=', LexemType::LexemEQ},
        {'.', LexemType::LexemPoint},
        {',', LexemType::LexemComma},
        {';', LexemType::LexemSemicolon},
        {'/', LexemType::LexemDiv},
        {'(', LexemType::LexemLeftBracket},
        {')', LexemType::LexemRightBracket},
        {'+', LexemType::LexemSum},
        {'-', LexemType::LexemMinus},
        {'*', LexemType::LexemMult},
    };

    // множество символов
    const set<Token::Char> Token::mOtherSymbol{ '=', '.', ',', ';', '/', '(',  ')', '+', '-', '*',  '!', '?', ' ', '[', ']', ':', '\t', '\n', '"', '&', '|', '$', '@', '#', '<', '>' , '\\'};

    // операция вывода
    ostream& operator<<(ostream& out, const Lexem& lexem) {
        switch (lexem.type) {
        case LexemBooleanConst:
        case LexemIntegerConst:
        case LexemStringConst:
        case LexemIdentifier: {
            auto it = Token::mTokenForPrint.find(lexem.type);
            out << (*it).second << " (" << lexem.value << ")";
            break;
        }
        default: auto it = Token::mTokenForPrint.find(lexem.type);
            out << (*it).second;
            break;
        }
        return out;
    }

    bool Token::HasLexem() {
        if (curHasLexem) { return true; }
        if (!curInput) { return false; } // если нет входного потока - ошибка

        Char c;
        curLexem.value.clear(); // очищаем значение текущей лексемы
        curState = &Token::bInitial; // задаем обработчик начального состояния

        do {
            c = curInput.get(); // считываем символ
            // проверяме является ли символ разрешенным, иначе формируем ошибку
            if (!IsLegalChar(c)) {
                throw logic_error("Oh, no! - line: " + to_string(iCurLineNum) + " - Illegal character: " + string(1, c));
            }


            // если символ конца строки, то накручиваем счетчик строк
            if (c == '\n') {
                iCurLineNum++;
            }
            // вызываем обработчик текущего состояния
            if ((this->*curState)(c)) {
                curHasLexem = true;  //устанавливаем статус -  лексема сформирована

                cout << curLexem << endl; // !!! - DEBUG PRINT - !!!


                return true; // выходим, т.к. лексема сформирована
            }
        } while (c != istream::traits_type::eof());
        return false;
    }

    bool Token::bInitial(Char c) {
        if ((c == istream::traits_type::eof()) || (c == ' ') || (c == '\n' || c == '\r')) { return false; }

        // если встретили 0
        if (c == '0') {
            curLexem.value = c; // сохраняем цифру
            curLexem.type = LexemType::LexemIntegerConst; // устанавливаем тип лексемы LexemIntegerConst
            return true;
        }

        // если встретили цифру от 1 до 9
        if (IsDigitNZ(c)) {
            curState = &Token::bNumber; // меняем обработчик состояния на bNumber
            curLexem.value = c; // сохраняем цифру
            curLexem.type = LexemType::LexemIntegerConst; // устанавливаем тип лексемы LexemIntegerConst
            return false;
        }

        // если встретили : или >
        if (c == ':') {
            curState = &Token::bAssign; // меняем обработчик состояния на bAssign
            return false;
        }

        // если встретили : или >
        if (c == '>') {
            curState = &Token::bMore; // меняем обработчик состояния на bMore
            return false;
        }

        // если встретили <
        if (c == '<') {
            curState = &Token::bNEQLess; // меняем обработчик состояния на bNEQLess
            return false;
        }

        // если встретили букву
        if (IsChar(c)) {
            curState = &Token::bWord; // меняем обработчик состояния на bWord
            curLexem.value = c;
            return false;
        }

        // если встретили одинарную кавычку
        if (c == '\'') {
            curLexem.value.clear();
            curLexem.type = LexemStringConst;
            curState = &Token::bStr; // меняем обработчик состояния на bStr
            return false;
        }

        // если встретили фигурную скобку
        if (c == '{') {
            curLexem.value.clear();
            curState = &Token::bComment; // меняем обработчик состояния на bComment
            return false;
        }

        // иначе проверяем является ли символ разделителем
        auto it = Token::mSeparators.find(c);  // ищем символ в карте разделителей
        if (it != mSeparators.end()) {
            curLexem.type = it->second; // если нашли, то устанавливаем тип лексемы
            return true;
        }

        // иначе формируем ошибку
        throw logic_error("Oh, no! - line: " + to_string(iCurLineNum) + " - You entered the wrong character " + string(1, c));
    }

    bool Token::bNumber(Char c) {
        // если цифра, то добавляем ее
        if (IsDigit(c)) {
            curLexem.value.push_back(c);
            return false; // лексема не сформирована
        }
        iCurLineNum -= (c == '\n');
        curInput.unget(); // если не цифра, то возвращаем обратно в поток
        return true; // лексема сформирована
    }

    // функция обработки состояния  Assign
    bool Token::bAssign(Char c) {
        if (c == '=') {
            curLexem.type = LexemType::LexemAssign;
        }
        else {
            curLexem.type = LexemType::LexemColon;
            iCurLineNum -= (c == '\n');
            curInput.unget();
        }
        return true;
    }

    // функция обработки состояния  More
    bool Token::bMore(Char c) {
        if (c == '=') {
            curLexem.type = LexemType::LexemMoreEQ;
        }
        else {
            curLexem.type = LexemType::LexemMore;
            iCurLineNum -= (c == '\n');
            curInput.unget();
        }
        return true;
    }

    bool Token::bNEQLess(Char c) {
        if (c == '=') {
            curLexem.type = LexemType::LexemLessEQ;
        }
        else if (c == '>') {
            curLexem.type = LexemType::LexemNEQ;
        }
        else {
            curLexem.type = LexemType::LexemLess;
            iCurLineNum -= (c == '\n');
            curInput.unget();
        }
        return true;
    }

    bool Token::bWord(Char c) {
        if (IsChar(c)) {
            curLexem.value.push_back(c);
            return false;
        }
        else if (IsDigit(c)) {
            curLexem.value.push_back(c);
            return false;
        }

        iCurLineNum -= (c == '\n');
        curInput.unget();

        std::string uppercase_value = curLexem.value;
        for (char& c : uppercase_value) {
            c = std::toupper(c);
        }

        auto it = Token::mKeyWords.find(uppercase_value);
        if (it != mKeyWords.end()) {
            curLexem.type = it->second; // устанавливаем тип лексемы отличную от логической константы
            curLexem.value = uppercase_value;
        } else {
            curLexem.type = LexemType::LexemIdentifier;
        }

        return true;
    }

    bool Token::bStr(Char c) {
        if (IsChar(c) || IsDigit(c) || IsOtherSymbol(c)) {
            curLexem.value.push_back(c);
            return false;
        }
        else if (c == '\'') {
            curState = &Token::bStrScreen;
            return false;
        }

        // иначе формируем ошибку
        throw logic_error("Oh, no! - line: " + to_string(iCurLineNum) + " - Second quote is expected ");
    }

    bool Token::bStrScreen(Char c) {
        if (c == '\'') {
            curLexem.value.push_back(c);
            curState = &Token::bStr;
            return false;
        }

        iCurLineNum -= (c == '\n');
        curInput.unget();
        return true;
    }

    bool Token::bComment(Char c) {
        if (IsChar(c) || IsDigit(c) || IsOtherSymbol(c) || c == '\'') {
            //curLexem.value.push_back(c);
            return false;
        }
        else if (c == '}') {
            curState = &Token::bInitial;
            return false;
        }

        // иначе формируем ошибку
        throw logic_error("Oh, no! - line: " + to_string(iCurLineNum) + " - Second brace in comment is expected! ");
    }

    Lexem Token::GetLexem() {
        if (!curHasLexem) {
            throw logic_error("Oh, no! - line: " + to_string(iCurLineNum) + " - You try to get lexem befor its creation!");
        }
        curHasLexem = false;
        return curLexem;
    }

    const Lexem& Token::LinkLexem() const {
        if (!curHasLexem) {
            throw logic_error("Oh, no! - line: " + to_string(iCurLineNum) + " - You try to get lexem befor its creation!");
        }
        return curLexem;
    }

} // --- end of namespace TokenNS

 // --- Синтаксический анализ
namespace ParserNS {
    using namespace execution;
    using namespace TokenNS;



    class Parser {
    public:
        explicit Parser(istream& input);
        int Run(); // возвращает число обработанных строк в случае успеха

    private:
        // todo - убрать глобальные переменные - done
        ValueType CurExprType; // текущий тип выражения, нужен для создания Операции
        LexemType CurOperType; // текущий тип операции, нужен для создания Операции
        map<VariableName, ValueType> Idents; // карта <имя переменной, тип переменной>, нужна для создания операций добавления переменных
        
        static const unordered_map<LexemType, string> mTokenForPrint;

        Token curToken;

        inline bool CheckLexem();  // проверка лексемы с геренацией ошибки
        inline bool CheckLexemType(const LexemType lexType);   // проверка типа лексемы
        inline void CheckLexemTypeWithError(const LexemType lexType); // проверка типа лексемы с геренацией ошибки по типу

        inline void CheckLexemAndLexemTypeWithError(const LexemType lexType);  // проверка лексемы и проверка типа лексемы с геренацией ошибки

        void Start(); // программа
        void DescrS(); // блок описания переменных
        void Descr(); // описание переменных
        void OperS(); // операторы
        void Oper(); // оператор
        void Expr(); // выражение
        void AExpr(); // выражение типа Add
        void MExpr(); // выражение типа Multiply
        void TExpr(); // выражение типа Terminal
    };

    // карта лексем для печати
    const unordered_map<LexemType, string>Parser::mTokenForPrint{
        {LexemType::LexemLeftSquare, "LexemLeftSquare" },
        {LexemType::LexemRightSquare, "LexemRightSquare" },
        {LexemType::LexemBooleanConst, "LexemBooleanConst" },
        {LexemType::LexemIntegerConst, "LexemIntegerConst" },
        {LexemType::LexemStringConst, "LexemStringConst" },
        {LexemType::LexemIdentifier, "LexemIdentifier" },
        {LexemType::LexemAnd, "LexemAnd" },
        {LexemType::LexemWrite, "LexemWrite" },
        {LexemType::LexemWriteln, "LexemWriteln" },
        {LexemType::LexemRead, "LexemRead" },
        {LexemType::LexemWhile, "LexemWhile" },
        {LexemType::LexemDo, "LexemDo" },
        {LexemType::LexemAssign, "LexemAssign" },
        {LexemType::LexemBegin, "LexemBegin" },
        {LexemType::LexemBoolean, "LexemBoolean" },
        {LexemType::LexemColon, "LexemColon" },
        {LexemType::LexemComma, "LexemComma" },
        {LexemType::LexemElse, "LexemElse" },
        {LexemType::LexemEnd, "LexemEnd" },
        {LexemType::LexemEQ, "LexemEQ" },
        {LexemType::LexemIf, "LexemIf" },
        {LexemType::LexemInteger, "LexemInteger" },
        {LexemType::LexemLeftBracket, "LexemLeftBracket" },
        {LexemType::LexemLess, "LexemLess" },
        {LexemType::LexemLessEQ, "LexemLessEQ" },
        {LexemType::LexemMinus, "LexemMinus" },
        {LexemType::LexemMore, "LexemMore" },
        {LexemType::LexemMoreEQ, "LexemMoreEQ" },
        {LexemType::LexemMult, "LexemMult" },
        {LexemType::LexemDiv, "LexemDiv" },
        {LexemType::LexemNEQ, "LexemNEQ" },
        {LexemType::LexemNot,"LexemNot"},
        {LexemType::LexemOr, "LexemOr"},
        {LexemType::LexemPoint,"LexemPoint"},
        {LexemType::LexemProgram,"LexemProgram"},
        {LexemType::LexemRightBracket,"LexemRightBracket"},
        {LexemType::LexemSemicolon,"LexemSemicolon"},
        {LexemType::LexemString,"LexemString"},
        {LexemType::LexemSum,"LexemSum"},
        {LexemType::LexemThen,"LexemThen"},
        {LexemType::LexemVar,"LexemVar"},
    };

    Parser::Parser(istream& input) : curToken(input) {}

    // проверка лексемы с геренацией ошибки
    inline bool Parser::CheckLexem() {
        if (!curToken.HasLexem()) {
            throw logic_error("Oh, no! - line: " + to_string(curToken.GetCurLine()) + " - Expected token ");
        }
        return true;
    }

    // проверка типа лексемы
    inline bool Parser::CheckLexemType(const LexemType lexType) {
        return (curToken.LinkLexem().type == lexType);
    }

    // проверка типа лексемы с генерацией ошибки по типу
    inline void Parser::CheckLexemTypeWithError(const LexemType lexType) {
        if (curToken.LinkLexem().type != lexType) {
            auto it = mTokenForPrint.find(lexType);
            throw logic_error("Oh, no! - line: " + to_string(curToken.GetCurLine()) + " - Expected " + (*it).second);
        }
    }

    // проверка лексемы и проверка типа лексемы с генерацией ошибки по типу
    // todo поменять название функции на более логичное - done
    inline void Parser::CheckLexemAndLexemTypeWithError(const LexemType lexType) { // проверка типа лексемы с геренацией ошибки по типу
        CheckLexem(); // считываем лексему и генерируем ошибку, если ее нет
        CheckLexemTypeWithError(lexType); // проверяем тип лексемы с генерацией ошибки
        curToken.GetLexem(); // записываем лексему
    }

    int Parser::Run() {
        Start();
        if (curToken.HasLexem()) {
            throw logic_error("Oh, no! - line: " + to_string(curToken.GetCurLine()) + " - Unexpected token " + curToken.LinkLexem().value);
        }
        return curToken.GetCurLine();
    }

    void Parser::Start() {

        CheckLexemAndLexemTypeWithError(LexemProgram); // проверяем лексему на тип Program и генерируем ошибку, если нет

        CheckLexem();
        if (CheckLexemType(LexemVar)) { // проверяем лексему на тип Var
            DescrS(); // переходим в состояние DescrS
        }
        OperS(); // переходим в состояние OperS

        CheckLexemAndLexemTypeWithError(LexemPoint); // проверяем лексему на тип точки
    }

    void Parser::DescrS() {
        CheckLexemAndLexemTypeWithError(LexemVar); // проверяем лексему на тип Var

        Descr();
        CheckLexem();
        while (CheckLexemType(LexemSemicolon)) { // пока ; считываем новые описания переменных
            curToken.GetLexem(); // curHasLexeme to false
            Descr();
            CheckLexem();
        }
    }

    void Parser::Descr() {
        stack<VariableName> LIdents; // временный стек имен переменных LIdents
        CheckLexem(); // считываем лексему и генерируем ошибку, если ее нет
        CheckLexemTypeWithError(LexemIdentifier); // проверка на идентификатор

        LIdents.push(curToken.LinkLexem().value); // добавляем имя переменной во временный стек имен LIdents

        Lexem lexem = curToken.GetLexem();

        CheckLexem(); // берем новую лексему
        // если больше одного идентификатора
        while (CheckLexemType(LexemComma)) {
            curToken.GetLexem(); // гасим запятую
            CheckLexem(); // считываем лексему и генерируем ошибку, если ее нет
            CheckLexemTypeWithError(LexemIdentifier); // проверка на идентификатор с ошибкой
            LIdents.push(curToken.LinkLexem().value); // добавляем имя переменной во временный стек имен LIdents
            curToken.GetLexem(); // гасим переменную

            CheckLexem();
        }

        CheckLexemAndLexemTypeWithError(LexemColon); // проверка на двоеточие

        // тип переменной
        CheckLexem(); // берем новую лексему
        lexem = curToken.LinkLexem();
        if (lexem.type != LexemInteger && lexem.type != LexemBoolean && lexem.type != LexemString) {
            throw logic_error("Oh, no! - line: " + to_string(curToken.GetCurLine()) + " - Invalid type ");
        }

        // определяем CurExprType для создания операции добавления переменной
        switch (lexem.type) {
        case(LexemInteger) : { CurExprType = Int; break; }
        case(LexemString) : { CurExprType = Str; break; }
        case(LexemBoolean) : { CurExprType = Logic; break; }
        }

        // записываем в вектор операций программы операции добавления переменных
        while (LIdents.size() != 0) {
            execution::operations.emplace_back(new AddVariableOperation(LIdents.top(), CurExprType,curToken.GetCurLine()));
            Idents.insert(make_pair(LIdents.top(), CurExprType)); // и добавляем пару переменная-тип в карту переменных для парсера
            LIdents.pop();
        }

        curToken.GetLexem(); // погасили статус старой лексемы
    }


    void Parser::OperS() {
        CheckLexem(); // считываем лексему и генерируем ошибку, если ее нет
        CheckLexemTypeWithError(LexemBegin); // проверка на Begin с генерацией ошибки
        Lexem lexem = curToken.GetLexem();

        Oper();
        CheckLexem(); // если больше одного оператора
        while (CheckLexemType(LexemSemicolon)) {
            curToken.GetLexem(); // гасим статус лексемы
            Oper();
            CheckLexem(); // берем новую лексему
        }

        CheckLexemTypeWithError(LexemEnd); // проверяем лексему на тип End с генерацией ошибки
        curToken.GetLexem(); // записываем лексему
    }

    void Parser::Oper() {
        CheckLexem(); // считываем лексему и генерируем ошибку, если ее нет
        Lexem lexem = curToken.LinkLexem();
        VariableName sNameTmp; // временная переменная для имени переменной в операторе присваивания и операторе read
        size_t Index_If, Index_AfterThen, Index_AfterElse; // индексы для оператора if
        size_t Index_BeforeWhile, Index_AfterWhile, Index_AfterCondition; // индексы для оператора while

        switch (lexem.type) {
        case LexemIdentifier: {

            // сохраняем имя переменной
            sNameTmp = lexem.value;

            curToken.GetLexem(); // тушим состояние лексемы
            CheckLexemAndLexemTypeWithError(LexemAssign); // проверка лексемы на тип присваивания
            Expr(); // переходим в состояние - выражение

            // добавляем операцию присваивания
            execution::operations.emplace_back(new AssignOperation(sNameTmp, curToken.GetCurLine()));

            break;
        }
        case LexemIf: {
            curToken.GetLexem(); // тушим состояние лексемы
            Expr();
            CheckLexemAndLexemTypeWithError(LexemThen); // проверка лексемы на тип Then
            Index_If = execution::operations.size(); // сохранили индекс, где будет размещена операция IFOperation(Index_AfterThen)
            execution::operations.emplace_back(nullptr); // резервируем ячейку под операцию IFOperation(Index_AfterThen)

            Oper();
            Index_AfterThen = execution::operations.size(); // сохранили индекс, после всех операций блока then

            CheckLexem(); // считываем лексему и генерируем ошибку, если ее нет
            // проверка лексемы на тип Else
            if (CheckLexemType(LexemElse)) {
                // если есть else
                 execution::operations.emplace_back(nullptr); // резервируем ячейку под операцию GOTO(Index_AfterElse)
                curToken.GetLexem(); // тушим статус лексемы
                Oper();
                Index_AfterElse = execution::operations.size(); // сохранили индекс, после всех операций блока else
                execution::operations[Index_If].reset(new IfOperation(Index_AfterThen+1, curToken.GetCurLine())); // размещаем операцию IFOperation(Index_AfterThen+1) в ячейке Index_If
                execution::operations[Index_AfterThen].reset(new GoOperation(Index_AfterElse, curToken.GetCurLine())); // размещаем операцию GoOperation(Index_AfterElse) в ячейке Index_AfterThen
            }
            else {
                // если нет else
                execution::operations[Index_If].reset(new IfOperation(Index_AfterThen, curToken.GetCurLine()));
            }
            break;
        }
        case LexemWhile: {
            curToken.GetLexem(); // тушим состояние лексемы
            Index_BeforeWhile = execution::operations.size(); // сохранили индекс на начало расчета условия цикла

            Expr();

            Index_AfterCondition = execution::operations.size(); // сохранили индекс после расчета условия цикла
            execution::operations.emplace_back(nullptr); // резервируем ячейку под операцию IFOperation(Index_AfterWhile)

            CheckLexemAndLexemTypeWithError(LexemDo); // проверка лексемы на тип Do
            OperS();
            execution::operations.emplace_back(new GoOperation(Index_BeforeWhile, curToken.GetCurLine())); // переход на новый расчет условия цикла
            Index_AfterWhile = execution::operations.size(); // сохранили индекс, куда будем переходить по лжи IFOperation(Index_AfterWhile)
            execution::operations[Index_AfterCondition].reset(new IfOperation(Index_AfterWhile, curToken.GetCurLine())); // размещаем операцию выхода из цикла в ячейке Index_AfterCondition

            break;
        }
        case LexemRead: {
            curToken.GetLexem(); // тушим состояние лексемы
            CheckLexemAndLexemTypeWithError(LexemLeftBracket);
            CheckLexem(); // считываем лексему и генерируем ошибку, если ее нет
            sNameTmp = curToken.LinkLexem().value; // сохраняем имя переменной для чтения
            CheckLexemTypeWithError(LexemIdentifier); // проверка лексемы на тип идентификатор
            curToken.GetLexem(); // записываем лексему

            execution::operations.emplace_back(new ReadOperation(sNameTmp, curToken.GetCurLine()));

            CheckLexemAndLexemTypeWithError(LexemRightBracket);
            break;
        }
        case LexemWrite: {
            curToken.GetLexem(); // тушим состояние лексемы
            CheckLexemAndLexemTypeWithError(LexemLeftBracket);
            Expr();
            CheckLexemAndLexemTypeWithError(LexemRightBracket);

            switch (CurExprType) {
            case Int: { execution::operations.emplace_back(new PrintOperation<int>(curToken.GetCurLine())); break; }
            case Str: { execution::operations.emplace_back(new PrintOperation<string>(curToken.GetCurLine())); break; }
            case Logic: { execution::operations.emplace_back(new PrintOperation<bool>(curToken.GetCurLine())); break; }
            }

            break;
        }
        case LexemWriteln: {
            curToken.GetLexem(); // тушим состояние лексемы
            CheckLexemAndLexemTypeWithError(LexemLeftBracket);
            Expr();
            CheckLexemAndLexemTypeWithError(LexemRightBracket);

            switch (CurExprType) {
            case Int: { execution::operations.emplace_back(new PrintlnOperation<int>(curToken.GetCurLine())); break; }
            case Str: { execution::operations.emplace_back(new PrintlnOperation<string>(curToken.GetCurLine())); break; }
            case Logic: { execution::operations.emplace_back(new PrintlnOperation<bool>(curToken.GetCurLine())); break; }
            }

            break;
        }

        case LexemBegin: {
            OperS();
            break;
        }
        default:
            throw logic_error("Oh, no! - line: " + to_string(curToken.GetCurLine()) + " - Invalid type ");
        }
    }

    void Parser::Expr() {
        AExpr();
        CheckLexem();
        // если есть сравнение
        Lexem lexem = curToken.LinkLexem();
        switch (lexem.type) {
        case LexemEQ: {
            curToken.GetLexem(); // тушим статус лексемы
            AExpr();
            if (CurExprType == Int) execution::operations.emplace_back(new EqualOperation<int>(curToken.GetCurLine()));
            if (CurExprType == Str) execution::operations.emplace_back(new EqualOperation<string>(curToken.GetCurLine()));
            if (CurExprType == Logic) execution::operations.emplace_back(new EqualOperation<bool>(curToken.GetCurLine()));
            break;
        }
        case LexemLess: {
            curToken.GetLexem(); // тушим статус лексемы
            AExpr();
            if (CurExprType == Int) execution::operations.emplace_back(new LessOperation<int>(curToken.GetCurLine()));
            if (CurExprType == Str) execution::operations.emplace_back(new LessOperation<string>(curToken.GetCurLine()));
            if (CurExprType == Logic) execution::operations.emplace_back(new LessOperation<bool>(curToken.GetCurLine()));
            break;
        }
        case LexemLessEQ: {
            curToken.GetLexem(); // тушим статус лексемы
            AExpr();
            if (CurExprType == Int) execution::operations.emplace_back(new LessOrEqualOperation<int>(curToken.GetCurLine()));
            if (CurExprType == Str) execution::operations.emplace_back(new LessOrEqualOperation<string>(curToken.GetCurLine()));
            if (CurExprType == Logic) execution::operations.emplace_back(new LessOrEqualOperation<bool>(curToken.GetCurLine()));
            break;
        }
        case LexemMore: {
            curToken.GetLexem(); // тушим статус лексемы
            AExpr();
            if (CurExprType == Int) execution::operations.emplace_back(new MoreOperation<int>(curToken.GetCurLine()));
            if (CurExprType == Str) execution::operations.emplace_back(new MoreOperation<string>(curToken.GetCurLine()));
            if (CurExprType == Logic) execution::operations.emplace_back(new MoreOperation<bool>(curToken.GetCurLine()));
            break;
        }
        case LexemMoreEQ: {
            curToken.GetLexem(); // тушим статус лексемы
            AExpr();
            if (CurExprType == Int) execution::operations.emplace_back(new MoreOrEqualOperation<int>(curToken.GetCurLine()));
            if (CurExprType == Str) execution::operations.emplace_back(new MoreOrEqualOperation<string>(curToken.GetCurLine()));
            if (CurExprType == Logic) execution::operations.emplace_back(new MoreOrEqualOperation<bool>(curToken.GetCurLine()));
            break;
        }

        case LexemNEQ: {
            curToken.GetLexem(); // тушим статус лексемы
            AExpr();
            if (CurExprType == Int) execution::operations.emplace_back(new NotEqualOperation<int>(curToken.GetCurLine()));
            if (CurExprType == Str) execution::operations.emplace_back(new NotEqualOperation<string>(curToken.GetCurLine()));
            if (CurExprType == Logic) execution::operations.emplace_back(new NotEqualOperation<bool>(curToken.GetCurLine()));
            break;
        }

        }
    }

    void Parser::AExpr() {
        MExpr();
        CheckLexem(); // если больше одного выражения

        while (CheckLexemType(LexemSum) || CheckLexemType(LexemMinus) || CheckLexemType(LexemOr)) {
            CurOperType = curToken.LinkLexem().type; // сохраняем тип операции

            curToken.GetLexem(); // гасим статус лексемы
            MExpr();
            CheckLexem(); // берем новую лексему

            if (CurOperType == LexemSum) {
                if (CurExprType == Int) execution::operations.emplace_back(new PlusOperation<int>(curToken.GetCurLine()));
                if (CurExprType == Str) execution::operations.emplace_back(new PlusOperation<string>(curToken.GetCurLine()));
            }

            if (CurOperType == LexemMinus) {
                execution::operations.emplace_back(new MinusOperation<int>(curToken.GetCurLine()));
            }
            if (CurOperType == LexemOr) {
                execution::operations.emplace_back(new OrOperation(curToken.GetCurLine()));
            }
        }
    }

    void Parser::MExpr() {
        TExpr();
        CheckLexem(); // если больше одного логического выражения
        while (CheckLexemType(LexemMult) || CheckLexemType(LexemDiv) || CheckLexemType(LexemAnd)) {
            CurOperType = curToken.LinkLexem().type; // сохраняем тип операции

            curToken.GetLexem(); // гасим статус лексемы
            TExpr();
            CheckLexem(); // берем новую лексему

            if (CurOperType == LexemMult) {
                execution::operations.emplace_back(new MulOperation<int>(curToken.GetCurLine()));
            }
            if (CurOperType == LexemDiv) {
                execution::operations.emplace_back(new DivOperation<int>(curToken.GetCurLine()));
            }
            if (CurOperType == LexemAnd) {
                execution::operations.emplace_back(new AndOperation(curToken.GetCurLine()));
            }
        }
    }

    void Parser::TExpr() {
        CheckLexem(); // считываем лексему и генерируем ошибку, если ее нет
        Lexem lexem = curToken.GetLexem();

        switch (lexem.type) {
        case LexemIdentifier: {

            // вставить проверку существования переменной --- ADD

            CurExprType = Idents[lexem.value]; // берем тип переменной и записываем в CurExprType
            execution::operations.emplace_back(new VariableOperation(lexem.value, curToken.GetCurLine())); // добавляем операцию взятия значения переменной
            break;
        }
        case LexemIntegerConst: {
            CurExprType = Int;
            execution::operations.emplace_back(new ValueOperation(stoi(lexem.value), curToken.GetCurLine())); // добавляем целочисленную константу
            break;
        }
        case LexemBooleanConst: {
            CurExprType = Logic;
            execution::operations.emplace_back(new ValueOperation(lexem.value == "TRUE", curToken.GetCurLine())); // добавляем логическую константу
            break;
        }
        case LexemStringConst: {
            CurExprType = Str;
            execution::operations.emplace_back(new ValueOperation(lexem.value, curToken.GetCurLine())); // добавляем строковую константу
            break;
        }
        case LexemLeftBracket: {
            Expr();
            CheckLexemAndLexemTypeWithError(LexemRightBracket);
            break;
        }
        case LexemMinus: {
            TExpr();
            CurExprType = Int;
            execution::operations.emplace_back(new UnaryMinusOperation<int>(curToken.GetCurLine()));
            break;
        }
        case LexemNot: {
            TExpr();
            CurExprType = Logic;
            execution::operations.emplace_back(new NotOperation(curToken.GetCurLine()));
            break;
        }
        default:
            throw logic_error("Oh, no! - line: " + to_string(curToken.GetCurLine()) + " - Error in expression ");
        }
    }

} // end of namespace ParserNS


int main(int argc, char* argv[]) {

    using namespace ParserNS;

    if (argc != 2) {
        cerr << "Nothing to parse. Thanks bro!" << endl;
        return 1;
    }
    try {
        ifstream input(argv[1]);
        if (!input) {
            cerr << "Oh no! - Error open file" <<  argv[1]  << endl;
            return 1;
        }

        cout << "Parsing file: " << argv[1] << endl;

        // проверка синтаксического анализатора
        Parser parser(input);
        int iCntLines = parser.Run();
        cout << "Oh, yeah! - " << iCntLines << " lines were parsed correctly. Good job bro!" << endl;

        // выполнение ПОЛИЗа
        try {
            while (context.operation_index < execution::operations.size()) {
                const auto& operation = execution::operations[context.operation_index];
                ++context.operation_index;
                operation->Do(context);
            }
        }
        catch (const std::exception& e) {
            std::cout << "error: " << e.what() << " - Error on line - " << context.iCurLine << std::endl;
        }

        return 0;
    }

    catch (const exception& eError) {
        cerr << eError.what() << endl;
        return 1;
    }
}