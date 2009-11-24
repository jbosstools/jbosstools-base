package org.jboss.tools.common.el.core;

import org.eclipse.osgi.util.NLS;

public class ElCoreMessages extends NLS {
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
	
	public static String EL_RENAME_PROCESSOR_LOCATION_NOT_FOUND;
	public static String EL_RENAME_PROCESSOR_OUT_OF_SYNC_FILE;
	public static String EL_RENAME_PROCESSOR_ERROR_PHANTOM_FILE;
	public static String EL_RENAME_PROCESSOR_ERROR_READ_ONLY_FILE;
	public static String RENAME_EL_VARIABLE_PROCESSOR_TITLE;
	public static String RENAME_EL_VARIABLE_PROCESSOR_CAN_NOT_FIND_CONTEXT_VARIABLE;
	
	static {
		// initialize resource bundle
		NLS.initializeMessages(BUNDLE_NAME, ElCoreMessages.class);
	}

	private ElCoreMessages() {
	}
}
