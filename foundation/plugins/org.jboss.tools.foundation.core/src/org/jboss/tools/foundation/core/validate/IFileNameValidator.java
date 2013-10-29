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

import org.eclipse.core.runtime.IStatus;

/**
 *
 */
public interface IFileNameValidator {
	/**
	 * An integer representing the validator should fail on any error or filesystem-specific failure
	 */
	int ERROR_ON_ALL = 0xFFFF;
	
	/**
	 * An integer representing the validator should fail only on the local filesystem type
	 */
	int ERROR_ON_LOCAL_FS = 0x1;
	
	/**
	 * An integer representing the validator should fail for errors against windows
	 */
	int ERROR_ON_WINDOWS = 0x2;
	
	/**
	 * An integer representing the validator should fail for errors against mac 
	 */
	int ERROR_ON_MAC = 0x4;
	
	
	
    /**
     * Validate the given filename
     *
     * @param fileName
     *
     * @return status indicating the result of the validity of the filename
     */
    IStatus validate(String fileName);

    
    /**
     * Validate the given filename for the given systems
     *
     * @param fileName
     * @param systems an integer representing some subset of possible systems
     * @return status indicating the result of the validity of the filename
     */
    IStatus validate(String fileName, int systems);

    
    /**
     * Set the parent directory which will be used in the filename validation.
     * In most cases, the default of java's temporary directory should be
     * sufficient but if the filename is going to be used on a different filesystem
     * then in may be prudent to test on this alternative filesystem.
     *
     * @param directory
     */
    void setParentDirectory(String directory);

    /**
     * Adds a character that should not be used in a filename.
     *
     * This character will be checked in addition to the minimum
     * list detailed in RESERVED_CHARACTERS.
     *
     * @param character
     */
    void addReservedCharacter(Character character);

    /**
     * Adds a word that should not be used as a filename.
     *
     * Once added, this word cannot appear as either a single filename or
     * a filename with a suffix.
     *
     * @param reservedWord
     */
    void addReservedWord(String reservedWord);

}
