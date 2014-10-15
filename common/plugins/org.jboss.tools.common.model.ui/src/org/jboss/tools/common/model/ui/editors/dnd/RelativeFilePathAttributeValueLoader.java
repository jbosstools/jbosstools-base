/*******************************************************************************
 * Copyright (c) 2014 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.common.model.ui.editors.dnd;

import org.eclipse.core.resources.IFile;
import org.jboss.tools.common.web.WebUtils;

/**
 * @author Alexey Kazakov
 */
public class RelativeFilePathAttributeValueLoader extends AbsoluteFilePathAttributeValueLoader {

	public RelativeFilePathAttributeValueLoader(String pathAttributeName,
			String widthAttributeName, String heightAttributeName) {
		super(pathAttributeName, widthAttributeName, heightAttributeName);
	}

	@Override
	protected String getPath(IFile context, IFile resource) {
		return WebUtils.getWebPath(context, resource);
	}
}