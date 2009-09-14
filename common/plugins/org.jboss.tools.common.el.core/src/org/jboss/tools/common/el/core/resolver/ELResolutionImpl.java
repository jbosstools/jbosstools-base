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

import java.util.ArrayList;
import java.util.List;

import org.eclipse.jdt.core.IJavaElement;
import org.jboss.tools.common.el.core.model.ELExpression;

/**
 * @author Alexey Kazakov
 */
public class ELResolutionImpl implements ELResolution {

	protected ELContext context;
	protected List<ELSegment> segments = new ArrayList<ELSegment>();
	protected ELExpression operand;

	/* (non-Javadoc)
	 * @see org.jboss.tools.common.el.core.resolver.ELResolution#findSegmentByJavaElement(org.eclipse.jdt.core.IJavaElement)
	 */
	public ELSegment findSegmentByJavaElement(IJavaElement element) {
		// TODO
		return null;
	}

	/* (non-Javadoc)
	 * @see org.jboss.tools.common.el.core.resolver.ELResolution#findSegmentByOffset(int)
	 */
	public ELSegment findSegmentByOffset(int offcet) {
		// TODO
		return null;
	}

	/* (non-Javadoc)
	 * @see org.jboss.tools.common.el.core.resolver.ELResolution#getContext()
	 */
	public ELContext getContext() {
		return context;
	}

	/* (non-Javadoc)
	 * @see org.jboss.tools.common.el.core.resolver.ELResolution#getSegments()
	 */
	public List<ELSegment> getSegments() {
		return segments;
	}

	/**
	 * Adds a segment
	 * @param segment
	 */
	public void addSegment(ELSegment segment) {
		segments.add(segment);
	}

	/* (non-Javadoc)
	 * @see org.jboss.tools.common.el.core.resolver.ELResolution#getSourceOperand()
	 */
	public ELExpression getSourceOperand() {
		return operand;
	}

	/* (non-Javadoc)
	 * @see org.jboss.tools.common.el.core.resolver.ELResolution#getUnresolvedSegment()
	 */
	public ELSegment getUnresolvedSegment() {
		// TODO
		return null;
	}

	/* (non-Javadoc)
	 * @see org.jboss.tools.common.el.core.resolver.ELResolution#hasUnresolvedSegment()
	 */
	public boolean hasUnresolvedSegment() {
		// TODO
		return false;
	}
}