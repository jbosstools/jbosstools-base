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
package org.jboss.tools.foundation.core.test.validate.impl;

import junit.framework.TestCase;
import org.eclipse.core.runtime.IStatus;
import org.jboss.tools.foundation.core.validate.IFileNameValidator;
import org.jboss.tools.foundation.core.validate.impl.FileNameValidator;
import org.junit.Test;

/**
 * Test class for the {@link FileNameValidator}
 */
public class TestFileNameValidator extends TestCase {

    private static final String WINDOWS = "WINDOWS"; //$NON-NLS-1$

    private static final String MAC = "MAC"; //$NON-NLS-1$

    /**
     * @return true if this system is running a Window OS, otherwise false
     */
    private boolean isWindows() {
        return getOSValue().contains(WINDOWS);
    }

    /**
     * @return true if this system is running a Mac OS, otherwise false
     */
    private boolean isMac() {
        return getOSValue().contains(MAC);
    }

    /**
     * @return value representation of host OS
     */
    private String getOSValue() {
        return System.getProperty("os.name").toUpperCase(); //$NON-NLS-1$
    }

    /**
     * Test validation of valid filenames
     */
    @Test
    public void testValidateValidFileNames() {
        String[] validFileNames = {
            "test345.xml", "test-345.txt", "test_345.doc", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
            "test.xmi", "test.txt", "test.xml", "test.csv", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
        };

        IFileNameValidator validator = new FileNameValidator();

        for (String fileName : validFileNames) {
            IStatus status = validator.validate(fileName);
            assertEquals(IStatus.OK, status.getSeverity());
        }
    }

    /**
     * Test the validation of suffixed and non-suffixed filenames
     */
    @Test
    public void testValidateSuffixes() {
        String[] fileNamesNoSuffix = {
            "test", "test345", "test-345", "test_345", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
        };

        IFileNameValidator validator = new FileNameValidator();

        for (String fileName : fileNamesNoSuffix) {
            IStatus status = validator.validate(fileName);
            if (isWindows()) {
                // Should fail since suffixes are preferred on windows
                assertEquals(IStatus.ERROR, status.getSeverity());
            } else {
                // Should warn since the filename is not portable
                assertEquals(IStatus.WARNING, status.getSeverity());
            }
        }
    }

    /**
     * Tests the validation of filenames using the built-in universal reserved characters
     */
    @Test
    public void testValidateReservedCharacters() {
        IFileNameValidator validator = new FileNameValidator();

        for (char c : IFileNameValidator.RESERVED_CHARACTERS) {
            StringBuilder builder = new StringBuilder("te"); //$NON-NLS-1$
            builder.append(c);
            builder.append("st.xmi"); //$NON-NLS-1$

            IStatus status = validator.validate(builder.toString());

            // Should fail since reserved characters are forbidden on windows
            assertEquals(IStatus.ERROR, status.getSeverity());
        }
    }

    /**
     * Tests the validation of filenames using the built-in mac reserved characters
     */
    @Test
    public void testValidateMacReservedCharacters() {
        IFileNameValidator validator = new FileNameValidator();

        for (char c : IFileNameValidator.MAC_RESERVED_CHARACTERS) {
            StringBuilder builder = new StringBuilder("te"); //$NON-NLS-1$
            builder.append(c);
            builder.append("st.xmi"); //$NON-NLS-1$

            IStatus status = validator.validate(builder.toString());
            if (isMac()) {
                // Should fail since reserved characters are forbidden on windows
                assertEquals(IStatus.ERROR, status.getSeverity());
            } else {
                // Should warn since the filename is not portable
                assertEquals(IStatus.WARNING, status.getSeverity());
            }
        }
    }

    /**
     * Tests the validation of filenames using the built-in windows reserved characters
     */
    @Test
    public void testValidateWindowsReservedCharacters() {
        IFileNameValidator validator = new FileNameValidator();

        for (char c : IFileNameValidator.WIN_RESERVED_CHARACTERS) {
            StringBuilder builder = new StringBuilder("te"); //$NON-NLS-1$
            builder.append(c);
            builder.append("st.xmi"); //$NON-NLS-1$

            IStatus status = validator.validate(builder.toString());
            if (isWindows()) {
                // Should fail since reserved characters are forbidden on windows
                assertEquals(IStatus.ERROR, status.getSeverity());
            } else {
                // Should warn since the filename is not portable
                assertEquals(IStatus.WARNING, status.getSeverity());
            }
        }
    }

    /**
     * Tests the loading and validation of added reserved characters
     */
    @Test
    public void testValidateLoadedReservedCharacters() {
        IFileNameValidator validator = new FileNameValidator();

        char[] reservedChars = new char[] {
            ')', '(', '[', ']', '@', '~', '#'
        };

        for (char reservedChar : reservedChars) {
            validator.addReservedCharacter(reservedChar);
        }

        for (char c : reservedChars) {
            StringBuilder builder = new StringBuilder("te"); //$NON-NLS-1$
            builder.append(c);
            builder.append("st.xmi"); //$NON-NLS-1$

            IStatus status = validator.validate(builder.toString());

            // Should always fail since the user added these reserved characters
            assertEquals(IStatus.ERROR, status.getSeverity());
        }
    }

    /**
     * Tests the validation of filenames using the built-in universal reserved words
     */
    @Test
    public void testValidateReservedWords() {
        IFileNameValidator validator = new FileNameValidator();

        for (String word : IFileNameValidator.RESERVED_WORDS) {
            StringBuilder builder = new StringBuilder(word);
            builder.append(".xmi"); //$NON-NLS-1$

            IStatus status = validator.validate(builder.toString());
            assertEquals(IStatus.ERROR, status.getSeverity());
        }
    }

    /**
     * Tests the validation of filenames using the built-in reserved words
     */
    @Test
    public void testValidateWindowsReservedWords() {
        IFileNameValidator validator = new FileNameValidator();

        for (String word : IFileNameValidator.WIN_RESERVED_WORDS) {
            StringBuilder builder = new StringBuilder(word);
            builder.append(".xmi"); //$NON-NLS-1$

            IStatus status = validator.validate(builder.toString());
            if (isWindows()) {
                // Should fail since reserved words are forbidden on windows
                assertEquals(IStatus.ERROR, status.getSeverity());
            } else {
                // Should warn since the filename is not portable
                assertEquals(IStatus.WARNING, status.getSeverity());
            }
        }
    }

    /**
     * Tests the loading of reserved words and validation of them.
     * All reserved words, whether singurlarly or with an extension,
     * should fail the validator.
     */
    @Test
    public void testValidateLoadedReservedWords() {
        IFileNameValidator validator = new FileNameValidator();

        String[] reservedWords = new String[] {
            "Select", "From", "TEMP", "SYS", "SYSADMIN" //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$
        };

        for (String reservedWord : reservedWords) {
            validator.addReservedWord(reservedWord);
        }

        for (String reservedWord : reservedWords) {
            // Test the word
            IStatus status = validator.validate(reservedWord);

            // Should always fail since the user added these reserved characters
            assertEquals(IStatus.ERROR, status.getSeverity());
        }
    }
}
