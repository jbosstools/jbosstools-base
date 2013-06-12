/******************************************************************************* 
 * Copyright (c) 2013 Red Hat, Inc. 
 * Distributed under license by Red Hat, Inc. All rights reserved. 
 * This program is made available under the terms of the 
 * Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html 
 * 
 * Contributors: 
 * Red Hat, Inc. - initial API and implementation 
 ******************************************************************************/
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
