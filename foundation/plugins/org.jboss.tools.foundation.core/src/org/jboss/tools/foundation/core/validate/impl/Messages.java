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

import org.eclipse.osgi.util.NLS;

public class Messages extends NLS {
    private static final String BUNDLE_NAME = Messages.class.getPackage().getName() + ".messages";  //$NON-NLS-1$

    public static String FileNameValidatorNullFileName;

    public static String FileNameValidatorTooLong;

    public static String FileNameValidatorEndsFinalCharacter;

    public static String FileNameValidatorNullCharacter;

    public static String FileNameValidatorReservedCharacter;

    public static String FileNameValidatorReservedWord;

    public static String FileNameValidatorNoSuffix;

    public static String FileNameValidatorFailedToCreateTestFile;

    public static String FileNameValidatorValidationWarning;

    public static String FileNameValidatorValidationFailed;

    public static String FileNameValidatorValidationSuccessful;

    static {
        // initialize resource bundle
        NLS.initializeMessages(BUNDLE_NAME, Messages.class);
    }

    private Messages() {
    }
}
