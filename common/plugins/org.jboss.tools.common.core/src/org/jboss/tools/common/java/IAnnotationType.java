/*******************************************************************************
  * Copyright (c) 2011 - 2012 Red Hat, Inc.
  * Distributed under license by Red Hat, Inc. All rights reserved.
  * This program is made available under the terms of the
  * Eclipse Public License v1.0 which accompanies this distribution,
  * and is available at http://www.eclipse.org/legal/epl-v10.html
  *
  * Contributor:
  *     Red Hat, Inc. - initial API and implementation
  ******************************************************************************/
package org.jboss.tools.common.java;

import java.util.List;

import org.eclipse.jdt.core.IType;

/**
 * Common interface for an annotation interface.
 * 
 * @author Viacheslav Kabanovich
 * 
 */
public interface IAnnotationType {

	/**
	 * Returns the corresponding IType of the annotation type.
	 * 
	 * @return the corresponding IType
	 */
	IType getSourceType();

	/**
	 * Returns all the available annotations which are declared for this
	 * interface.
	 * 
	 * @return all the available annotations which are declared for this
	 *         interface
	 */
	List<IAnnotationDeclaration> getAnnotationDeclarations();

	/**
	 * Returns the annotations with given type name.
	 * 
	 * @param typeName
	 * @return the annotations with given type name
	 */
	IAnnotationDeclaration getAnnotationDeclaration(String typeName);

}