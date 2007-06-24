/*******************************************************************************
 * Copyright (c) 2007 Exadel, Inc. and Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Exadel, Inc. and Red Hat, Inc. - initial API and implementation
 ******************************************************************************/ 
package org.jboss.tools.common.text.ext.util;

import org.eclipse.core.resources.IFile;
import org.eclipse.jface.text.IDocument;
import org.eclipse.wst.sse.core.internal.provisional.IModelManager;
import org.eclipse.wst.sse.core.internal.provisional.IStructuredModel;
import org.eclipse.wst.sse.core.internal.provisional.StructuredModelManager;
import org.eclipse.wst.xml.core.internal.provisional.document.IDOMDocument;
import org.eclipse.wst.xml.core.internal.provisional.document.IDOMModel;
import org.jboss.tools.common.model.XModel;
import org.jboss.tools.common.text.ext.hyperlink.AbstractHyperlink;
import org.w3c.dom.Document;

public class StructuredModelWrapper {
	IStructuredModel model = null;
	
	public StructuredModelWrapper() {
	}
	
	public void init(IDocument id) {
		model = getModelManager().getExistingModelForRead(id);
	}
	
	public Document getDocument() {
		return (model instanceof IDOMModel) ? ((IDOMModel) model).getDocument() : null;
	}
	
	public XModel getXModel() {
		return AbstractHyperlink.getXModel(model);
	}
	
	public IFile getFile() {
		return AbstractHyperlink.getFile(model);
	}
	
	public void dispose() {
		if(model != null) {
			model.releaseFromRead();
			model = null;
		}
	}

	protected IModelManager getModelManager() {
		return StructuredModelManager.getModelManager();
	}
	
	public String getContentTypeIdentifier() {
		return model.getContentTypeIdentifier();
	}

}
