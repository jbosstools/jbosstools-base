/*******************************************************************************
 * Copyright (c) 2007-2012 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/ 
package org.jboss.tools.common.text.ext.hyperlink;

import java.util.List;
import java.util.Properties;

import org.eclipse.core.resources.IFile;
import org.eclipse.jface.text.IRegion;
import org.jboss.tools.common.model.XModel;
import org.jboss.tools.common.model.project.IPromptingProvider;
import org.jboss.tools.common.model.project.PromptingProviderFactory;

/**
 * @author Jeremy
 */
public abstract class XModelBasedHyperlink extends AbstractHyperlink {
	protected Properties requestProperties = null;

	/** 
	 * @see com.ibm.sse.editor.AbstractHyperlink#doHyperlink(org.eclipse.jface.text.IRegion)
	 */
	protected void doHyperlink(IRegion region) {
		IFile documentFile = getFile();
		XModel xModel = getXModel(documentFile);
		if (xModel == null) {
			openFileFailed();
			return;
		}

		IPromptingProvider provider = PromptingProviderFactory.WEB;

		requestProperties = getRequestProperties(region);
		requestProperties.put(IPromptingProvider.FILE, documentFile);

		List list = provider.getList(xModel, getRequestMethod(), requestProperties.getProperty("prefix"), requestProperties); //$NON-NLS-1$
		if (list != null && list.size() >= 1) {
			openFileInEditor((String)list.get(0));
			return;
		}
		String error = requestProperties.getProperty(IPromptingProvider.ERROR); 
		if ( error != null && error.length() > 0) {
			openFileFailed();
		}
	}
	
	protected abstract String getRequestMethod();

	protected abstract Properties getRequestProperties(IRegion region);
}
