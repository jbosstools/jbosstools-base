package org.jboss.tools.common.model.plugin;

import org.eclipse.osgi.util.NLS;

public class ModelMessages extends NLS {
	private static final String BUNDLE_NAME = "org.jboss.tools.common.model.plugin.messages";//$NON-NLS-1$
	
	private ModelMessages() {}

	public static String WARNING;

	public static String UNKNOUN_ENTITY;
	public static String CREATION_ENTITY_FAILURE;

	public static String SET_ATTRIBUTE_FAILURE;
	public static String CONSTRAINT_NOLOOP;
	public static String CONSTRAINT_INTEGER;
	public static String CONSTRAINT_NONEMPTY;
	public static String CONSTRAINT_NO_JAVA_KEYWORD;
	public static String CONSTRAINT_JAVA_NAME;
	public static String CONSTRAINT_INTEGER_OR_LIST;
	public static String CONSTRAINT_RED_HAT_TEMPLATE_NAME;
	public static String CONSTRAINT_XML_NAME;
	public static String CONSTRAINT_IS_NOT_IN_LIST;
	public static String IS_A_RESERVED_WORD;

	public static String ATTRIBUTE_REQUIRED;
	public static String BrowserHelper_DialogTitleRun;

	public static String BrowserHelper_EnterValidPath;

	public static String BrowserHelper_InternetBrowserNotSet;

	public static String OBJECT_CREATION_FAILURE;
	public static String OBJECT_ADDING_FAILURE;
	public static String CONTAINS_OBJECT_1;
	public static String CONTAINS_OBJECT_2;

	public static String OK;
	public static String Cancel;
	public static String Finish;
	public static String Error;
	public static String Yes;
	public static String No;

	static {
		NLS.initializeMessages(BUNDLE_NAME, ModelMessages.class);
	}

}
