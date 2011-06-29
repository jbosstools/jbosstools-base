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
package org.jboss.tools.common.java;

import java.util.List;

import org.eclipse.jdt.core.IType;

/**
 * Represents a wrapper for IType object which could be parameted.
 * For example if we have some method
 *    List<String> getList() {...}
 * then IParametedType for return type of this method will wrap List<> and its signature.
 */
public interface IParametedType {

	/**
	 * Returns the corresponding IType of the declaration. May be null.
	 * 
	 * @return the corresponding IType of the declaration.
	 */
	IType getType();

	/**
	 * Returns signature of the declaration.
	 * 
	 * @return signature of the declaration
	 */
	public String getSignature();

	/**
	 * Returns true if the type is a primitive type.
	 * 
	 * @return true if the type is a primitive type
	 */
	boolean isPrimitive();

	/**
	 * Returns the simple name of the type. In case of IType this method will return the short name of the type.
	 * If this type is primitive then the method will return the name of the primitive type.
	 * 
	 * @return the simple name of the type.
	 */
	String getSimpleName();

	/**
	 * Returns type parameters
	 * 
	 * @return type parameters
	 */
	List<? extends IParametedType> getParameters();
}