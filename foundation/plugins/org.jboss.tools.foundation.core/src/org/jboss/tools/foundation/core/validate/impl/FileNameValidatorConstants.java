/*******************************************************************************
 * Copyright (c) 2013 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.foundation.core.validate.impl;

public interface FileNameValidatorConstants {
    /**
     * Null character
     */
    String NUL = "\u0000"; //$NON-NLS-1$

    /**
     * Dot character
     */
    String DOT = "."; //$NON-NLS-1$

    /**
     * Space character
     */
    String SPACE = " "; //$NON-NLS-1$

    /**
     * Characters not valid on any OS in a filename
     */
    char[] RESERVED_CHARACTERS = { '/' };

    /**
     * Characters not valid on Mac
     */
    char[] MAC_RESERVED_CHARACTERS = { ':' };

    /**
     * Characters not valid on Windows
     */
    char[] WIN_RESERVED_CHARACTERS = {
        '<', '>', ':', '"', '\\', '|', '?', '*', '[', ']', '\u0001', '\u0002',
        '\u0003', '\u0004', '\u0005', '\u0006', '\u0007', '\u0008', '\u0009', '\r', '\u000B', '\u000C', '\n', '\u000E', '\u000F',
        '\u0010', '\u0011', '\u0012', '\u0013', '\u0014', '\u0015', '\u0016', '\u0017', '\u0018', '\u0019', '\u001A', '\u001B',
        '\u001C', '\u001D', '\u001E', '\u001F', '\u007F'
    };

    /**
     * Words that cannot be used as names of files on any OS
     */
    String[] RESERVED_WORDS = {DOT, DOT + DOT};

    /**
     * Words that cannot be used as names of files.
     * Primarily, these are not allowed on Windows.
     */
    String[] WIN_RESERVED_WORDS = {
        "CON", "PRN", //$NON-NLS-1$ //$NON-NLS-2$
        "AUX", "CLOCK$", //$NON-NLS-1$ //$NON-NLS-2$
        "NUL", "COM1", //$NON-NLS-1$ //$NON-NLS-2$
        "COM2", "COM3", //$NON-NLS-1$ //$NON-NLS-2$
        "COM4", "COM5", //$NON-NLS-1$ //$NON-NLS-2$
        "COM6", "COM7", //$NON-NLS-1$ //$NON-NLS-2$
        "COM8", "COM9", //$NON-NLS-1$ //$NON-NLS-2$
        "LPT1", "LPT2", //$NON-NLS-1$ //$NON-NLS-2$
        "LPT3", "LPT4", //$NON-NLS-1$ //$NON-NLS-2$
        "LPT5", "LPT6", //$NON-NLS-1$ //$NON-NLS-2$
        "LPT7", "LPT8", "LPT9" //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
    };

}
