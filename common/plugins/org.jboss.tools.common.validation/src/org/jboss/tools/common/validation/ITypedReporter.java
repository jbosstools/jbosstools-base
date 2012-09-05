/******************************************************************************* 
 * Copyright (c) 2012 Red Hat, Inc. 
 * Distributed under license by Red Hat, Inc. All rights reserved. 
 * This program is made available under the terms of the 
 * Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html 
 * 
 * Contributors: 
 * Red Hat, Inc. - initial API and implementation 
 ******************************************************************************/
package org.jboss.tools.common.validation;

import java.util.Collection;
/**
 * Validatior-Type dependent reporter
 * 
 * @author Victor V. Rubezhny
 */
public interface ITypedReporter {
	
	/**
	 * If a validator processes whole file at once it adds types of its messages using this method
	 * 
	 * @param type
	 */
	void addTypeForFile(String type);
	
	/**
	 * Returns the types of messages from a 'file'-validators
	 *  
	 * @return
	 */
	Collection<String> getTypesForFile();
	
	/**
	 * If a validator processes text regions one by one it adds types of its messages using this method
	 * 
	 * @param type
	 */
	void addTypeForRegion(String type);

	/**
	 * Returns the types of messages from a 'region'-validators
	 *  
	 * @return
	 */
	Collection<String> getTypesForRegion();
}
