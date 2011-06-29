/******************************************************************************* 
 * Copyright (c) 2010 Red Hat, Inc. 
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

import org.jboss.tools.common.text.ITextSourceReference;

/**
 * Represents a model element that can be annotated.
 * 
 * @author Alexey Kazakov
 */
public interface IAnnotated {

	/**
	 * Get all annotations of the element.
	 * 
	 * @return all annotations of the element, or an empty list if no
	 *         annotations are present
	 */
	List<IAnnotationDeclaration> getAnnotations();

	/**
	 * Get element annotation of a certain annotation type.
	 * 
	 * @param annotationTypeName
	 *            the name of the annotation type
	 * @return the element annotation of the given annotation type, or a null
	 *         value
	 */
	IAnnotationDeclaration getAnnotation(String annotationTypeName);

	/**
	 * This method is similar to getAnnotation(String annotationTypeName). But
	 * JDT doesn't have API for getting IAnnotation from method parameters so
	 * this method is preferable for method parameters.
	 * 
	 * @param annotationTypeName
	 *            the name of the annotation type
	 * @return the text source reference of the annotation of the given annotation type, or a null
	 *         value
	 */
	ITextSourceReference getAnnotationPosition(String annotationTypeName);

	/**
	 * Determine if the element has an annotation of a certain annotation type.
	 * 
	 * @param annotationTypeName
	 *            the annotation type to check for
	 * @return true if the element has an annotation of the given annotation
	 *         type, or false otherwise
	 */
	boolean isAnnotationPresent(String annotationTypeName);
}