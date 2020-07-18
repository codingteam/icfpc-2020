using Executor.Tree;

namespace Executor.Reducers
{
    public class ApReducer : IReducer
    {
        public TreeNode Reduce(TreeNode node)
        {
            if (node is Application)
            {

            }
            throw new System.NotImplementedException();
        }
    }

    public class UserDefinedFunctionReducer : IReducer
    {
        public TreeNode Reduce(TreeNode node)
        {
            if (node is Application application &&
                application.Func is UserDefinedFunction userDefinedFunction)
                return new Application(userDefinedFunction.Body,
                    application.Arg);
            return node;
        }
    }

    public class SCombinatorReducer : IReducer
    {
        public TreeNode Reduce(TreeNode node)
        {
            //ap (ap (ap s x0) x1) x2   =   ap ap x0 x2 ap x1 x2
            if (node is Application a1
                && a1.Func is Application a2
                && a2.Func is Application a3
                && a3.Func is SCombinator)
                return new Application(
                    new Application(a3.Arg, a1.Arg),
                    new Application(a2.Arg, a1.Arg)
                );
            return node;
        }
    }

    public class IfZeroReducer : IReducer
    {
        public TreeNode Reduce(TreeNode node)
        {
            //ap (ap (ap if0 0) x1) x2   =   x0
            if (node is Application a1
                && a1.Func is Application a2
                && a2.Func is Application a3
                && a3.Func is IfZero)
            {
                var arg = AstReducer.Reduce(a3.Arg);
                if (arg is Number number)
                {
                    return number.Value == 0 ? a2.Arg : a1.Arg;
                }
            }

            return node;
        }
    }
}
