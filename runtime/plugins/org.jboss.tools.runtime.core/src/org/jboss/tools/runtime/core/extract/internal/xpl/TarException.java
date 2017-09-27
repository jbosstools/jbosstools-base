/*******************************************************************************
 * Copyright (c) 2004, 2007 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 * IBM Corporation - initial API and implementation
 *******************************************************************************/
package org.jboss.tools.runtime.core.extract.internal.xpl;

import java.io.IOException;
/**
 * Exception generated upon encountering corrupted tar files.
 * <p>
 * Copied from org.eclipse.ui.internal.wizards.datatransfer.
 * </p>
 * 
 *  @deprecated this implementation doesn't HeaderPax, please use Apache Commons Compress
 */
@Deprecated
public class TarException extends IOException {
	/**
	 * Generated serial version UID for this class.
	 */
	private static final long serialVersionUID = 2886671254518853528L;

    /**
     * Constructs a TarException without a detail string.
     */
    public TarException() {
    	super();
    }
	
	/**
     * Constructs a TarException with the specified detail string.
     *
     * @param s the detail string
     */
    public TarException(String s) {
    	super(s);
    }
}