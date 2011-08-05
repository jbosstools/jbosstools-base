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
package org.jboss.tools.common.el.core.resolver;

import java.util.List;

import org.eclipse.core.resources.IResource;
import org.jboss.tools.common.el.core.parser.LexicalToken;
import org.jboss.tools.common.text.ITextSourceReference;

/**
 * Describes a segment of EL operand.
 * @author Alexey Kazakov
 */
public interface ELSegment {

	/**
	 * @return source EL token.
	 */
	LexicalToken getToken();

	/**
	 * @return true if the segment has been resolved.
	 */
	boolean isResolved();

	/**
	 * @return resource of underlying object.
	 */
	IResource getResource();

	/**
	 * @return source reference of underlying object.
	 */
	ITextSourceReference getSourceReference();

	/**
	 * Returns references to objects that can be opened from this segment.
	 * If there are no references, empty array is returned. The returned object is never null.
	 * 
	 * @return references to objects that can be opened from this segment.
	 */
	IOpenableReference[] getOpenable();

	/**
	 * @return underlying variables.
	 */
	List<IVariable> getVariables();

	/**
	 * Returns false if EL validator should ignore this segment and don't mark it as a problem even if the segemtn is not resolved.
	 * @return
	 */
	boolean isValidatable();
}