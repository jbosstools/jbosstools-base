/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.jboss.tools.foundation.security;

import org.eclipse.osgi.util.NLS;

/**
 *
 */
public class Messages extends NLS {
    private static final String BUNDLE_NAME = Messages.class.getPackage().getName() + ".messages"; //$NON-NLS-1$

    public static String SecureDescription;
    public static String DescriptionTitle;
    public static String MessageLogin;
    public static String MessageLoginChange;
    public static String MessageEmptyPassword;
    public static String MessageNoMatch;
    public static String LabelPassword;
    public static String LabelConfirm;
    public static String ButtonLogin;
    public static String ButtonExit;
    public static String DialogTitle;
    public static String PasswordChangeTitle;
    public static String ShowPassword;
    public static String NoDigestPassword;
    public static String LocationGroup;

    static {
        // initialize resource bundle
        NLS.initializeMessages(BUNDLE_NAME, Messages.class);
    }

    private Messages() {
    }
}
