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
package org.jboss.tools.foundation.core.expressions;

/**
 * A variable resolver is an object capable of returning
 * a value for a given variable.  
 * @since 1.1
 */
public interface IVariableResolver {
	
	/**
	 * Return a string representation of this resolved variable. 
	 * The argument may serve as a context, or as a default value
	 * 
	 * @param variable
	 * @param argument
	 * @return
	 */
	public String resolve(String variable, String argument);
}
