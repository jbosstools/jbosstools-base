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
package org.jboss.tools.foundation.core.test.validate;

import junit.framework.TestCase;
import org.jboss.tools.foundation.core.validate.FileNameValidator;
import org.junit.Test;

/**
 * Test class for the {@link FileNameValidator}
 */
public class TestFileNameValidator extends TestCase {

    /**
     * Test validation of valid filenames
     */
    @Test
    public void testValidateValidFileNames() {
        String[] validFileNames = {
            "test", "test345", "test-345", "test_345", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
            "test.xmi", "test.txt", "test.xml", "test.csv", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
        };

        FileNameValidator validator = new FileNameValidator();

        for (String fileName : validFileNames) {
            assertTrue(validator.validate(fileName));
        }
    }

    /**
     * Test the validation with the suffix check turned on
     */
    @Test
    public void testValidateWithSuffixCheck() {
        String[] validFileNames = {
            "test", "test345", "test-345", "test_345", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
        };

        String[] validFileNamesWithSuffix = {
            "test.xmi", "test.txt", "test.xml", "test.csv", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
        };

        FileNameValidator validator = new FileNameValidator();
        validator.setCheckSuffix(true);

        for (String fileName : validFileNames) {
            assertFalse(validator.getFailureMessage(), validator.validate(fileName));
        }

        for (String fileName : validFileNamesWithSuffix) {
            assertTrue(validator.validate(fileName));
        }
    }

    /**
     * Tests the validation of filenames using the built-in reserved characters
     */
    @Test
    public void testValidateReservedCharacters() {
        FileNameValidator validator = new FileNameValidator();

        for (char c : FileNameValidator.RESERVED_CHARACTERS) {
            StringBuilder builder = new StringBuilder("te"); //$NON-NLS-1$
            builder.append(c);
            builder.append("st"); //$NON-NLS-1$

            assertFalse(validator.getFailureMessage(), validator.validate(builder.toString()));
        }
    }

    /**
     * Tests the loading and validation of added reserved characters
     */
    @Test
    public void testValidateLoadedReservedCharacters() {
        FileNameValidator validator = new FileNameValidator();

        char[] reservedChars = new char[] {
            ')', '(', '[', ']', '@', '~', '#'
        };

        for (char reservedChar : reservedChars) {
            validator.addReservedCharacter(reservedChar);
        }

        for (char c : reservedChars) {
            StringBuilder builder = new StringBuilder("te"); //$NON-NLS-1$
            builder.append(c);
            builder.append("st"); //$NON-NLS-1$

            assertFalse(validator.getFailureMessage(), validator.validate(builder.toString()));
        }
    }

    /**
     * Tests the loading of reserved words and validation of them.
     * All reserved words, whether singurlarly or with an extension,
     * should fail the validator.
     */
    @Test
    public void testValidateLoadedReservedWords() {
        FileNameValidator validator = new FileNameValidator();

        String[] reservedWords = new String[] {
            "Select", "From", "TEMP", "SYS", "SYSADMIN" //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$
        };

        for (String reservedWord : reservedWords) {
            validator.addReservedWord(reservedWord);
        }

        for (String reservedWord : reservedWords) {
            // Test the word
            assertFalse(validator.getFailureMessage(), validator.validate(reservedWord));

            // Test the word with a suffix
            assertFalse(validator.getFailureMessage(), validator.validate(reservedWord + ".xmi")); //$NON-NLS-1$
        }
    }
}
