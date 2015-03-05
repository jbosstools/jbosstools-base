/*******************************************************************************
 * Copyright (c) 2010 JVM Monitor project. All rights reserved. 
 * 
 * This code is distributed under the terms of the Eclipse Public License v1.0
 * which is available at http://www.eclipse.org/legal/epl-v10.html
 *******************************************************************************/
package org.jboss.tools.common.jdt.debug;

import org.eclipse.osgi.util.NLS;

/** 
 * The messages.
 */
public final class Messages extends NLS {

    /** The bundle name. */
    private static final String BUNDLE_NAME = "org.jboss.tools.common.jdt.debug.messages";//$NON-NLS-1$

    static {
        NLS.initializeMessages(BUNDLE_NAME, Messages.class);
    }

    /**
     * The constructor.
     */
    private Messages() {
        // do not instantiate
    }

    // tools preference page


    // error log messages

    /** */
    public static String addingClassPathFailedMsg;

    /** */
    public static String addingLibraryPathFailedMsg;

    /** */
    public static String classPathAddedMsg;

    /** */
    public static String jdkRootDirectoryNotFoundMsg;

    /** */
    public static String jdkRootDirectoryFoundMsg;

    /** */
    public static String jdkRootDirectoryNotEnteredMsg;

    /** */
    public static String notSupportedMsg;

    /** */
    public static String selectJdkRootDirectoryMsg;

    /** */
    public static String directoryNotExistMsg;

    /** */
    public static String notJdkRootDirectoryMsg;
    
    public static String NoJdkDirectoryFoundMsg;
    
    public static String getMainClassNameFailed;
}
