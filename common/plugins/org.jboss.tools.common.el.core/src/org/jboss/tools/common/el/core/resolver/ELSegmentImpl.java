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

import org.eclipse.core.resources.IResource;
import org.jboss.tools.common.el.core.model.ELInvocationExpression;
import org.jboss.tools.common.model.project.ext.ITextSourceReference;

/**
 * @author Alexey Kazakov
 */
public class ELSegmentImpl implements ELSegment {

	protected IResource resource;
	protected ITextSourceReference sourceReference;
	protected ELInvocationExpression token;
	protected boolean resolved = false;

	/* (non-Javadoc)
	 * @see org.jboss.tools.common.el.core.resolver.ELSegment#getResource()
	 */
	public IResource getResource() {
		return resource;
	}

	/* (non-Javadoc)
	 * @see org.jboss.tools.common.el.core.resolver.ELSegment#getSourceReference()
	 */
	public ITextSourceReference getSourceReference() {
		return sourceReference;
	}

	/* (non-Javadoc)
	 * @see org.jboss.tools.common.el.core.resolver.ELSegment#getToken()
	 */
	public ELInvocationExpression getToken() {
		return token;
	}

	/* (non-Javadoc)
	 * @see org.jboss.tools.common.el.core.resolver.ELSegment#isResolved()
	 */
	public boolean isResolved() {
		return resolved;
	}

	/**
	 * @param resource the resource to set
	 */
	public void setResource(IResource resource) {
		this.resource = resource;
	}

	/**
	 * @param sourceReference the sourceReference to set
	 */
	public void setSourceReference(ITextSourceReference sourceReference) {
		this.sourceReference = sourceReference;
	}

	/**
	 * @param token the token to set
	 */
	public void setToken(ELInvocationExpression token) {
		this.token = token;
	}

	/**
	 * @param resolved the resolved to set
	 */
	public void setResolved(boolean resolved) {
		this.resolved = resolved;
	}
}