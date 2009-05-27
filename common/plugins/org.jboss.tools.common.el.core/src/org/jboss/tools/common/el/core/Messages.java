package org.jboss.tools.common.el.core;

import org.eclipse.osgi.util.NLS;

public class Messages extends NLS {
	private static final String BUNDLE_NAME = "org.jboss.tools.common.el.core.messages"; //$NON-NLS-1$
	public static String CallRule_ExpectingCommaOrRParen;
	public static String CallRule_ExpectingRBrace;
	public static String CallRule_ExpectingRBracket;
	public static String CallRule_ExpectingRParen;
	public static String CallRule_UnexpectedLParen;
	public static String ExpressionRule_CannotStartWithBinaryOp;
	public static String ExpressionRule_CannotStartWithInstanceof;
	public static String ExpressionRule_ExpectingExpression;
	public static String ExpressionRule_ExpectingJavaName;
	public static String JavaNameTokenDescription_Name;
	public static String OperationRule_ExpectingRBrace;
	public static String OperationRule_ExpectingRBracket;
	public static String OperationRule_ExpectingRParen;
	public static String OperationTokenDescription_Name;
	public static String PrimitiveValueTokenDescription_ArgMustResolveToIntegerOrString;
	public static String PrimitiveValueTokenDescription_Name;
	public static String StringTokenDescription_Name;
	public static String WhiteSpaceTokenDescription_Name;
	static {
		// initialize resource bundle
		NLS.initializeMessages(BUNDLE_NAME, Messages.class);
	}

	private Messages() {
	}
}
