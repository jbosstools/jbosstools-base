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
package org.jboss.tools.foundation.core.validate;

import java.io.File;
import java.util.ArrayList;
import java.util.Collection;

/**
 * Validates a filename by creating a temporary file with the given name
 * on the underlying filesystem. In addition, checks are made on reserved
 * characters and words.
 */
public class FileNameValidator {

    /**
     * Not valid on Windows and make no sense to be included in a filename
     */
    public static final char[] RESERVED_CHARACTERS = {
        '<', '>', ':' ,'"', '/', '\\', '|', '?', '*'
    };

    /**
     * Root directory for testing files
     */
    private String parentDir = System.getProperty("java.io.tmpdir"); //$NON-NLS-1$

    /**
     * Message if the last call to {@link #validate(String)} failed.
     * Overwritten by each call to {@link #validate(String)} and will
     * be null if successful.
     */
    private String failMessage = null;

    /**
     * Flag to indicate whether the file name should be checked for a suffix
     *
     * False by default.
     */
    private boolean checkSuffix = false;

    /**
     * Collection of reserved characters that should not appear in filenames
     */
    private Collection<Character> reservedChars = new ArrayList<Character>();

    /**
     * Collection of reserved words that should not appear as filenames
     */
    private Collection<String> reservedWords = new ArrayList<String>();

    /**
     * Test whether the given file name contains any of the set of
     * reserved characters
     *
     * @param fileName
     *
     * @return true if a reserved character is present, false otherwise.
     */
    private boolean containsReservedChars(String fileName) {
        for (char c : RESERVED_CHARACTERS) {
            if (fileName.indexOf(c) > -1)
                return true;
        }

        for (char c : reservedChars) {
            if (fileName.indexOf(c) > -1)
                return true;
        }

        return false;
    }

    /**
     * Tests whether the given filename is one of the reserved words
     *
     * @param fileName
     * @return
     */
    private boolean isReservedWord(String fileName) {
        for (String reservedWord : reservedWords) {
            if (fileName.equalsIgnoreCase(reservedWord))
                return true;

            String[] nameComponents = fileName.split("\\."); //$NON-NLS-1$
            for (String component : nameComponents) {
                if (reservedWord.equalsIgnoreCase(component))
                    return true;
            }
        }

        return false;
    }

    /**
     * Test whether given file name contains a dot implying the presence
     * of a suffix. This is important for Windows file names but not Unix.
     *
     * @param fileName
     *
     * @return true if a suffix is present or if this check has been disabled, false otherwise.
     */
    private boolean hasSuffix(String fileName) {
        if (! checkSuffix) {
            // check suffix disabled
            return true;
        }

        return fileName.contains("."); //$NON-NLS-1$
    }

    /**
     * Validate the given filename
     *
     * @param fileName
     *
     * @return true if the given filename is valid, false otherwise
     */
    public boolean validate(String fileName) {
        failMessage = null;

        if (fileName == null) {
            failMessage = "The given file name is null"; //$NON-NLS-1$
            return false;
        }

        if (containsReservedChars(fileName)) {
            failMessage = "Contains reserved character"; //$NON-NLS-1$
            return false;
        }

        if (isReservedWord(fileName)) {
            failMessage = "Filename '" + fileName + "' is a reserved word"; //$NON-NLS-1$ //$NON-NLS-2$
            return false;
        }

        if (! hasSuffix(fileName)) {
            failMessage = "Filename lacks a suffix which is required in this environment"; //$NON-NLS-1$
            return false;
        }

        File tempTestFile = new File(parentDir, fileName);
        try {
            if (tempTestFile.createNewFile()) {
                tempTestFile.delete();
                failMessage = null;
                return true;
            }
        } catch (Exception ex) {
          failMessage = ex.getLocalizedMessage();
          return false;
       }

        return false;
    }

    /**
     * Get the failure message if the last call to {@link #validate(String)} failed
     * or null if successful.
     *
     * @return failure message or null
     */
    public String getFailureMessage() {
        return failMessage;
    }

    /**
     * Set the value of the check suffix option. If the check suffix flag is disabled
     * then filenames that do NOT have a suffix will be considered valid.
     *
     * Filenames without a suffix are valid on Unix but not on Windows
     *
     * @param status
     */
    public void setCheckSuffix(boolean status) {
        this.checkSuffix = status;
    }

    /**
     * Set the parent directory which will be used in the filename validation.
     * In most cases, the default of java's temporary directory should be
     * sufficient but if the filename is going to be used on a different filesystem
     * then in may be prudent to test on this alternative filesystem.
     *
     * @param directory
     */
    public void setParentDirectory(String directory) {
        this.parentDir = directory;
    }

    /**
     * Adds a character that should not be used in a filename.
     *
     * This character will be checked in addition to the minimum
     * list detailed in RESERVER_CHARACTERS.
     *
     * @param character
     */
    public void addReservedCharacter(Character character) {
        reservedChars.add(character);
    }

    /**
     * Adds a word that should not be used as a filename.
     *
     * Once added, this word cannot appear as either a single filename or
     * a filename with a suffix.
     *
     * @param reservedWord
     */
    public void addReservedWord(String reservedWord) {
        reservedWords .add(reservedWord);
    }
}
