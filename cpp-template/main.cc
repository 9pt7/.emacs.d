/*! A test class */
class Afterdoc_Test
{
public:
    /** An enum type.
     *  The documentation block cannot be put after the enum!
     */
    enum EnumType
    {
        EVal1,     /**< enum value 1 */
        EVal2      /**< enum value 2 */
    };
    void member();   //!< a member function.

protected:
    int value;       /*!< an integer value */
};

int main(int argc, const char *argv[])
{

}
