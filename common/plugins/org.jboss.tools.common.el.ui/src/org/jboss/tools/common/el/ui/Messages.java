/******************************************************************************* 
 * Copyright (c) 2007 Red Hat, Inc. 
 * Distributed under license by Red Hat, Inc. All rights reserved. 
 * This program is made available under the terms of the 
 * Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html 
 * 
 * Contributors: 
 * Red Hat, Inc. - initial API and implementation 
 ******************************************************************************/ 

package org.jboss.tools.common.el.ui;

import org.eclipse.osgi.util.NLS;

/**
 * @author eskimo
 *
 */
public class Messages {
	
    /** 
     * The resource bundle name. 
     * */
    private static final String BUNDLE_NAME = "org.jboss.tools.common.el.ui.messages";//$NON-NLS-1$

    static {
        // load message values from bundle file
        NLS.initializeMessages(BUNDLE_NAME, Messages.class);       
    }
    
    /**
     * The Constructor.
     */
    private Messages(){}

	public static String SUBSTITUTED_EL_EXPRESSIONS;

}
