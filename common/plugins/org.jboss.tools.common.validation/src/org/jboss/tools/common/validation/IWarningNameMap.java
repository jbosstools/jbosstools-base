/******************************************************************************* 
 * Copyright (c) 2011 Red Hat, Inc. 
 * Distributed under license by Red Hat, Inc. All rights reserved. 
 * This program is made available under the terms of the 
 * Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html 
 * 
 * Contributors: 
 * Red Hat, Inc. - initial API and implementation 
 ******************************************************************************/
package org.jboss.tools.common.validation;

/**
 * This interface represents a Map between the validation preference ID
 * and all the available warning names which can be used in @SuppressWarnings("...")
 * to suppress the corresponding validation.
 * @author Alexey Kazakov
 */
public interface IWarningNameMap {

	static final String EXTENSION_POINT_ID = "org.jboss.tools.common.validation.warnings"; //$NON-NLS-1$

	/**
	 * Returns array of warning names which can be used in @SuppressWarnings("...")
	 * which are mapped to the full preference ID of the corresponding validation rule.
	 * The first name of the result array is preferred and the rest of names are optional. 
	 * Returns null if no warning name found for this preference ID.
	 * @param preferenceID
	 * @return
	 */
	String[] getWarningNames(String preferenceID);
}