/*******************************************************************************
 * Copyright (c) 2010 JVM Monitor project. All rights reserved. 
 * 
 * This code is distributed under the terms of the Eclipse Public License v1.0
 * which is available at http://www.eclipse.org/legal/epl-v10.html
 *******************************************************************************/
package org.jboss.tools.common.jdt.debug.tools;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.Status;
import org.jboss.tools.common.jdt.debug.RemoteDebugActivator;

/**
 * The JVM core exception.
 */
public class ToolsCoreException extends CoreException {

    /** The serial version UID. */
    private static final long serialVersionUID = 1L;

    /**
     * The constructor.
     * 
     * @param severity
     *            The severity (e.g. IStatus.ERROR)
     * @param message
     *            the message
     * @param t
     *            The exception
     */
    public ToolsCoreException(int severity, String message, Throwable t) {
        super(new Status(severity, RemoteDebugActivator.PLUGIN_ID, message, t));
    }
}
