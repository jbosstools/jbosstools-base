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

import org.eclipse.core.resources.IResource;
import org.eclipse.jdt.core.IAnnotation;
import org.eclipse.jdt.core.IMember;
import org.eclipse.jdt.core.IMemberValuePair;
import org.eclipse.jdt.core.IType;
import org.jboss.tools.common.text.ITextSourceReference;

/**
 * Represents an annotation declaration. For example, in CDI, a qualifier or a scope
 * declaration of a bean/injection/produce method.
 * 
 * @author Alexey Kazakov
 */
public interface IAnnotationDeclaration extends ITextSourceReference {

	/**
	 * Returns the member value pairs.
	 * 
	 * @return the member value pairs
	 */
	IMemberValuePair[] getMemberValuePairs();

	/**
	 * Convenience method that allows to get one member value without enumerating pairs.
	 * @param name
	 * @return member value, may be null, e.g. when value is an expression
	 */
	Object getMemberValue(String name);

	/**
	 * Returns the member which is annotated by this declaration.
	 * 
	 * @return the member which is annotated by this declaration
	 */
	IMember getParentMember();

	/**
	 * Returns the corresponding type name of the annotation.
	 * 
	 * @return the corresponding type name of the annotation
	 */
	String getTypeName();

	/**
	 * Returns the corresponding IType of the annotation. May be null.
	 * 
	 * @return the corresponding IType of the annotation
	 */
	IType getType();

	/**
	 * Returns the corresponding annotation. May be null.
	 * 
	 * @return the corresponding annotation
	 */
	IAnnotationType getAnnotation();

	/**
	 * Returns underlying Java annotation if this declaration is based on Java source.
	 * Returns null if this declaration is based on xml source.
	 * 
	 * @return underlying Java annotation if it exists
	 */
	IAnnotation getJavaAnnotation();

	/**
	 * Returns resource which contains this declaration
	 * @return
	 */
	IResource getResource();
}