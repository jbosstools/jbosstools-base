/******************************************************************************
 * Copyright (c) 2005 BEA Systems, Inc.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *    Konstantin Komissarchik - initial API and implementation
 ******************************************************************************/

package org.jboss.tools.common.core.classpath;

import java.util.ArrayList;

import org.eclipse.core.runtime.IPath;
import org.eclipse.jdt.core.IClasspathAttribute;
import org.eclipse.jdt.core.JavaCore;

/**
 * @author <a href="mailto:kosta@bea.com">Konstantin Komissarchik</a>
 */

public final class ClasspathDecorations {
	private IPath sourceAttachmentPath;
	private IPath sourceAttachmentRootPath;
	private ArrayList extraAttributes = new ArrayList();

	public IPath getSourceAttachmentPath() {
		return this.sourceAttachmentPath;
	}

	public void setSourceAttachmentPath(final IPath sourceAttachmentPath) {
		this.sourceAttachmentPath = sourceAttachmentPath;
	}

	public IPath getSourceAttachmentRootPath() {
		return this.sourceAttachmentRootPath;
	}

	public void setSourceAttachmentRootPath(final IPath sourceAttachmentRootPath) {
		this.sourceAttachmentRootPath = sourceAttachmentRootPath;
	}

	public IClasspathAttribute[] getExtraAttributes() {
		final IClasspathAttribute[] array = new IClasspathAttribute[this.extraAttributes
				.size()];

		return (IClasspathAttribute[]) this.extraAttributes.toArray(array);
	}

	public void setExtraAttributes(final IClasspathAttribute[] attrs) {
		for (int i = 0; i < attrs.length; i++) {
			this.extraAttributes.add(attrs[i]);
		}
	}

	public void addExtraAttribute(final String name, final String value) {
		final IClasspathAttribute attr = JavaCore.newClasspathAttribute(name,
				value);

		this.extraAttributes.add(attr);
	}

}
