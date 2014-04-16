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

import java.io.File;
import java.util.ArrayList;
import java.util.Collection;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.MultiStatus;
import org.eclipse.core.runtime.Status;
import org.jboss.tools.foundation.core.FoundationCorePlugin;
import org.jboss.tools.foundation.core.validate.IFileNameValidator;

/**
 * Validates a filename by creating a temporary file with the given name
 * on the underlying filesystem. In addition, checks are made on reserved
 * characters and words.
 */
public class FileNameValidator implements IFileNameValidator, FileNameValidatorConstants {

    private static final String WINDOWS = "WINDOWS"; //$NON-NLS-1$

    private static final String MAC = "MAC"; //$NON-NLS-1$

    private class ValidatorStatus extends MultiStatus {

        /**
         * Create new instance
         */
        public ValidatorStatus() {
            super(FoundationCorePlugin.PLUGIN_ID, IStatus.OK, Messages.FileNameValidatorValidationSuccessful, null);
        }

        @Override
        protected void setMessage(String message) {
            super.setMessage(message);
        }
    }

    /**
     * Root directory for testing files
     */
    private String parentDir = System.getProperty("java.io.tmpdir"); //$NON-NLS-1$

    /**
     * Collection of reserved characters that should not appear in filenames
     */
    private Collection<Character> reservedChars = new ArrayList<Character>();

    /**
     * Collection of reserved words that should not appear as filenames
     */
    private Collection<String> reservedWords = new ArrayList<String>();

    /**
     * @return given status is a failure
     */
    private boolean isFailure(IStatus status) {
        return status.matches(IStatus.ERROR);
    }

    /**
     * Create a failure status
     */
    private IStatus createFailureStatus(String message) {
        return new Status(IStatus.ERROR, FoundationCorePlugin.PLUGIN_ID, message);
    }

    /**
     * Set the warning message
     */
    private IStatus createWarningStatus(String message) {
        return new Status(IStatus.WARNING, FoundationCorePlugin.PLUGIN_ID, message);
    }

    /**
     * @return true if this system is running a Window OS, otherwise false
     */
    private boolean isWindows() {
        return getOSValue().contains(WINDOWS);
    }

    /**
     * @return true if this system is running a Window OS, otherwise false
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
     * Should we create a test file for this validation
     * 
     * @return
     */
    private boolean shouldCreateTestFile(int systems) {
    	if( failLocalFilesystem(systems) || 
    			(isWindows() && failWindows(systems)) ||
    			(isMac() && failMac(systems))) {
    			return true;
    	}
    	return false;
    	
    }
    
    /**
     * Given the systems to fail on, should we fail on windows
     * 
     * @param systems
     * @return
     */
    private boolean failLocalFilesystem(int systems) {
    	return (((systems & ERROR_ON_LOCAL_FS) == ERROR_ON_LOCAL_FS));
    }


    /**
     * Given the systems to fail on, should we fail on windows
     * 
     * @param systems
     * @return
     */
    private boolean failWindows(int systems) {
    	return (isWindows() && ((systems & ERROR_ON_LOCAL_FS) == ERROR_ON_LOCAL_FS)) || 
    			((systems & ERROR_ON_WINDOWS) == ERROR_ON_WINDOWS);
    }

    /**
     * Given the systems to fail on, should we fail on mac
     * 
     * @param systems
     * @return
     */
    private boolean failMac(int systems) {
    	return (isMac() && ((systems & ERROR_ON_LOCAL_FS) == ERROR_ON_LOCAL_FS)) || 
    			((systems & ERROR_ON_MAC) == ERROR_ON_MAC);
    }

    
    
    /**
     * Test whether the given file name contains the nul character.
     *
     * This is applicable to all systems.
     *
     * @param fileName
     *
     * @return ok status if test passes, otherwise a failure status
     */
    private IStatus testNullCharacter(String fileName) {
        if (fileName.contains(NUL)) {
            return createFailureStatus(Messages.FileNameValidatorNullCharacter);
        }

        return Status.OK_STATUS;
    }

    /**
     * File name should be less than or equal to 255 characters
     *
     * This is applicable to all systems.
     *
     * @param fileName
     *
     * @return ok status if test passes, otherwise a failure status
     */
    private IStatus testLength(String fileName) {
        if (fileName.length() > 255) {
            return createFailureStatus(Messages.FileNameValidatorTooLong);
        }

        return Status.OK_STATUS;
    }

    /**
     * Test whether the given file name contains any of the set of
     * reserved characters
     *
     * This is especially applicable to Windows and FAT file systems
     *
     * @param fileName
     * @param systems an integer representing systems to fail on
     *
     * @return ok status if test passes, otherwise a failure status
     */
    private IStatus testReservedChars(String fileName, int systems) {
        /*
         * User-added characters so test should fail if the file name
         * contains them
         */
        for (char c : reservedChars) {
            if (fileName.indexOf(c) > -1) {
                return createFailureStatus(Messages.FileNameValidatorReservedCharacter);
            }
        }

        /*
         * Test for universal reserved characters
         */
        for (char c : RESERVED_CHARACTERS) {
            if (fileName.indexOf(c) > -1) {
                return createFailureStatus(Messages.FileNameValidatorReservedCharacter);
            }
        }

        MultiStatus status = new MultiStatus(FoundationCorePlugin.PLUGIN_ID, IStatus.OK, "", null); //$NON-NLS-1$

        /*
         * Test for Mac reserved characters
         */
        for (char c : MAC_RESERVED_CHARACTERS) {
            if (fileName.indexOf(c) > -1) {
                if (failMac(systems) ) {
                    return createFailureStatus(Messages.FileNameValidatorReservedCharacter);
                } else {
                    status.add(createWarningStatus(Messages.FileNameValidatorReservedCharacter));
                }
            }
        }

        /*
         * Test for Windows reserved characters
         */
        for (char c : WIN_RESERVED_CHARACTERS) {
            if (fileName.indexOf(c) > -1) {
                if (failWindows(systems)) {
                    return createFailureStatus(Messages.FileNameValidatorReservedCharacter);
                } else {
                    status.add(createWarningStatus(Messages.FileNameValidatorReservedCharacter));
                }
            }
        }

        return status;
    }

    /**
     * Tests whether the given filename is one of the reserved words
     *
     * This is especially applicable to Windows
     *
     * @param fileName
     * @param systems an integer representing systems to fail on
     * @return ok status if test passes, otherwise a failure status
     */
    private IStatus testReservedWord(String fileName, int systems) {
        int lastDot = fileName.lastIndexOf(DOT);
        if (lastDot > -1)
            fileName = fileName.substring(0, lastDot);

        /*
         * User-added words so test should fail if the file name
         * contains them
         */
        for (String word : reservedWords) {
            if (word.equalsIgnoreCase(fileName)) {
                return createFailureStatus(Messages.FileNameValidatorReservedWord);
            }
        }

        /*
         * Test for universal reserved characters
         */
        for (String word : RESERVED_WORDS) {
            if (word.equalsIgnoreCase(fileName)) {
                return createFailureStatus(Messages.FileNameValidatorReservedWord);
            }
        }

        MultiStatus status = new MultiStatus(FoundationCorePlugin.PLUGIN_ID, IStatus.OK, "", null); //$NON-NLS-1$

        /*
         * Test for Windows reserved words
         */
        for (String word : WIN_RESERVED_WORDS) {
            if (word.equalsIgnoreCase(fileName)) {
                if (failWindows(systems)) {
                    return createFailureStatus(Messages.FileNameValidatorReservedWord);
                } else {
                    status.add(createWarningStatus(Messages.FileNameValidatorReservedWord));
                }
            }
        }

        return status;
    }

    /**
     * Test whether given file name contains a dot implying the presence
     * of a suffix.
     *
     * This is applicable to Windows.
     *
     * @param fileName
     *
     * @return ok status if test passes, otherwise a failure status
     */
    private IStatus testSuffix(String fileName, int systems) {
        int dotIndex = fileName.indexOf(DOT);
        if (dotIndex < 1 || dotIndex >= fileName.length() - 1) {
            // No suffix
            if (failWindows(systems)) {
                return createFailureStatus(Messages.FileNameValidatorNoSuffix);
            } else {
                return createWarningStatus(Messages.FileNameValidatorNoSuffix);
            }
        }

        return Status.OK_STATUS;
    }

    /**
     * File name cannot end with a period or space.
     *
     * This is applicable to Windows
     *
     * @param fileName
     *
     * @return ok status if test passes, otherwise a failure status
     */
    private IStatus testIllegalFinalCharacter(String fileName, int systems) {

        if (fileName.endsWith(DOT) || fileName.endsWith(SPACE)) {
            if (failWindows(systems)) {
                return createFailureStatus(Messages.FileNameValidatorEndsFinalCharacter);
            } else {
                return createWarningStatus(Messages.FileNameValidatorEndsFinalCharacter);
            }
        }

        return Status.OK_STATUS;
    }
    
    private void evaluate(IStatus testStatus, ValidatorStatus outputStatus) {
        if (! testStatus.isOK()) {
            // Only care about failures and warnings
            outputStatus.add(testStatus);

            switch (outputStatus.getSeverity()) {
                case IStatus.ERROR:
                    outputStatus.setMessage(Messages.FileNameValidatorValidationFailed);
                    break;
                case IStatus.WARNING:
                    outputStatus.setMessage(Messages.FileNameValidatorValidationWarning);
                    break;
            }
        }
    }

    /**
     * Validate the given filename
     *
     * @param fileName
     *
     * @return status indicating the result of the validity of the filename
     */
    @Override
    public IStatus validate(String fileName) {
    	return validate(fileName, ERROR_ON_LOCAL_FS);
    }
    
    public IStatus validate(String fileName, int systems) {
        if (fileName == null) {
            return createFailureStatus(Messages.FileNameValidatorNullFileName);
        }

        ValidatorStatus resultStatus = new ValidatorStatus();

        evaluate(testLength(fileName), resultStatus);
        if (isFailure(resultStatus))
            return resultStatus;

        evaluate(testNullCharacter(fileName), resultStatus);
        if (isFailure(resultStatus))
            return resultStatus;

        evaluate(testIllegalFinalCharacter(fileName, systems), resultStatus);
        if (isFailure(resultStatus))
            return resultStatus;

        evaluate(testReservedChars(fileName, systems), resultStatus);
        if (isFailure(resultStatus))
            return resultStatus;

        evaluate(testReservedWord(fileName, systems), resultStatus);
        if (isFailure(resultStatus))
            return resultStatus;

        evaluate(testSuffix(fileName, systems), resultStatus);
        if (isFailure(resultStatus))
            return resultStatus;

        
        if( shouldCreateTestFile(systems)) {
        
	        /*
	         * Test a temporary file can be created with the file name
	         */
	        File tempTestFile = new File(parentDir, fileName);
	        try {
	
	            if (tempTestFile.createNewFile()) {
	                tempTestFile.delete();
	            } else
	                resultStatus.add(createFailureStatus(Messages.FileNameValidatorFailedToCreateTestFile));
	
	        } catch (Exception ex) {
	            StringBuilder builder = new StringBuilder(Messages.FileNameValidatorFailedToCreateTestFile);
	            builder.append("\n"); //$NON-NLS-1$
	            builder.append(ex.getLocalizedMessage());
	            resultStatus.add(createFailureStatus(builder.toString()));
	       }
        }
        return resultStatus;
    }

    /**
     * Set the parent directory which will be used in the filename validation.
     * In most cases, the default of java's temporary directory should be
     * sufficient but if the filename is going to be used on a different filesystem
     * then in may be prudent to test on this alternative filesystem.
     *
     * @param directory
     */
    @Override
    public void setParentDirectory(String directory) {
        this.parentDir = directory;
    }

    /**
     * Adds a character that should not be used in a filename.
     *
     * This character will be checked in addition to the minimum
     * list detailed in RESERVED_CHARACTERS.
     *
     * @param character
     */
    @Override
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
    @Override
    public void addReservedWord(String reservedWord) {
        reservedWords .add(reservedWord);
    }
}
