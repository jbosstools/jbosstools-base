package org.jboss.tools.common.model.ui.objecteditor;

import org.eclipse.osgi.util.NLS;

public class Messages extends NLS {
	private static final String BUNDLE_NAME = "org.jboss.tools.common.model.ui.objecteditor.messages"; //$NON-NLS-1$
	public static String XChildrenEditor_Add;
	public static String XChildrenEditor_Delete;
	public static String XChildrenEditor_Down;
	public static String XChildrenEditor_Edit;
	public static String XChildrenEditor_Up;
	static {
		// initialize resource bundle
		NLS.initializeMessages(BUNDLE_NAME, Messages.class);
	}

	private Messages() {
	}
}
