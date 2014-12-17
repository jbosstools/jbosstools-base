/******************************************************************************* 
 * Copyright (c) 2009 Red Hat, Inc. 
 * Distributed under license by Red Hat, Inc. All rights reserved. 
 * This program is made available under the terms of the 
 * Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html 
 * 
 * Contributors: 
 * Red Hat, Inc. - initial API and implementation 
 ******************************************************************************/ 
package org.jboss.tools.common.validation;

import java.util.Set;

import org.eclipse.core.resources.IProject;

/**
 * Represents a set of projects which are being validated.  
 * @author Alexey Kazakov
 */
public interface IValidatingProjectSet {

	/**
	 * @return the root project which holds a link to validating context for this project set.
	 */
	IProject getRootProject();

	/**
	 * @return all the projects of the set.
	 */
	Set<IProject> getAllProjects();

	/**
	 * @return Root validating context which is associated with the root project.
	 */
	IProjectValidationContext getRootContext();

	/**
	 * Returns true if model related builder requested full validation.
	 * @return
	 */
	boolean isFullValidationRequired();

	/**
	 * Related builder requests full project validation with b=true;
	 * Validator on doing full validation clears this property with b=false;
	 * 
	 * @param b
	 */
	void setFullValidationRequired(boolean b);
}