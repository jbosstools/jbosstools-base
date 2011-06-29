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
package org.jboss.tools.common.java;

import org.eclipse.core.resources.IResource;
import org.eclipse.jdt.core.IMember;
import org.eclipse.jdt.core.IMemberValuePair;
import org.eclipse.jdt.core.IType;
import org.jboss.tools.common.text.ITextSourceReference;

/**
 * Hides IAnnotation and allows to subtitute another implementation than wrapper for IAnnotation.
 * 
 * @author Viacheslav Kabanovich
 *
 */
public interface IJavaAnnotation extends ITextSourceReference {

	/**
	 * Returns resource that declares this annotation.
	 * 
	 * @return resource that declares this annotation
	 */
	public IResource getResource();

	/**
	 * Returns fully qualified type name if resolved or element name otherwise.
	 * 
	 * @return fully qualified type name if resolved or element name otherwise
	 */
	public String getTypeName();

	/**
	 * Returns annotation type or null if it cannot be resolved.
	 * 
	 * @return annotation type or null if it cannot be resolved
	 */
	public IType getType();
	/**
	 * Returns Java element on which or for which this annotation was created.
	 * 
	 * @return Java element on which or for which this annotation was created
	 */
	public IMember getParentMember();

	/**
	 * Returns member value pairs as IAnnotation does.
	 * 
	 * @return member value pairs as IAnnotation does
	 */
	public IMemberValuePair[] getMemberValuePairs();

}
