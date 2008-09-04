/******************************************************************************* 
 * Copyright (c) 2007 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/

package org.jboss.tools.common.resref.messages;

import java.util.ResourceBundle;

import org.eclipse.osgi.util.NLS;

/**
 * The Class Messages.
 * 
 * @author Eugene Stherbin
 */
public final class Messages {
    
    /** The Constant BUNDLE_NAME. */
    private static final String BUNDLE_NAME = "org.jboss.tools.common.resref.messages.messages";//$NON-NLS-1$
    
    /** The f resource bundle. */
    private static ResourceBundle fResourceBundle;
    static {
        // load message values from bundle file
        NLS.initializeMessages(BUNDLE_NAME, Messages.class);       
    }
    
    /**
     * The Constructor.
     */
    private Messages(){}
    
    
    public static String INCLUDED_CSS_FILES;
    public static String INCLUDED_TAG_LIBS;
    public static String SUBSTITUTED_EL_EXPRESSIONS;
    public static String ACTUAL_RUN_TIME_ABSOLUTE_FOLDER;
}
