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

import java.io.IOException;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.ISafeRunnable;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.util.SafeRunnable;
import org.eclipse.wst.sse.core.StructuredModelManager;
import org.eclipse.wst.sse.core.internal.provisional.IModelManager;
import org.eclipse.wst.sse.core.internal.provisional.IStructuredModel;
import org.eclipse.wst.sse.core.internal.provisional.text.IStructuredDocument;
import org.eclipse.wst.xml.core.internal.provisional.document.IDOMDocument;
import org.eclipse.wst.xml.core.internal.provisional.document.IDOMModel;
import org.jboss.tools.common.model.XModel;
import org.jboss.tools.common.text.ext.ExtensionsPlugin;
import org.jboss.tools.common.text.ext.hyperlink.AbstractHyperlink;
import org.w3c.dom.Document;

public class StructuredModelWrapper {
	/**
	 * Interface to use with static <code>execute(IFile file, final Command command)</code>
	 * method. Use inner class if access to class' fields or final local variables 
	 * is required.
	 * 
	 * @see StructuredModelWrapper.execute(IFile file, final Command command)  
	 */
	public interface ICommand {
		/**
		 * Execute code related to <code>xmlDocument instance<code>.
		 * @param xmlDocument - document for file passed to <code>StructuredModelWrapper.execute</code>
		 *        method. It is never called with null value.
		 */
		void execute(IDOMDocument xmlDocument);
		
	}

	IStructuredModel model = null;
	
	public StructuredModelWrapper() {
	}
	
	public void init(IDocument id) {
		model = getModelManager().getExistingModelForRead(id);
	}

	public void init(IFile file) throws IOException, CoreException {
		model = getModelManager().getModelForRead(file);
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
	
	public String getBaseLocation() {
		return AbstractHyperlink.getBaseLocation(model);
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

	/**
	 * Static method to execute command with IDOMModel instance provided for
	 * file. Method obtains structured model instance from StructuredModelManager and release
	 * it in final block.
	 * <code><pre>
	 * final List&lt;TextProposal&gt; proposals = new ArrayList&lt;TextProposal&gt;();
	 * IFile file = context.getResource();
	 * StructuredModelWrapper.execute(file, new Command() {
	 *     public void execute(IDOMDocument xmlDocument) {
	 *         String text = xmlDocument.getNodeName(); 
	 *         proposals.add(new TextProposal(IMAGE,text,text,text.length(),text));
	 *     }
	 * });
	 * </pre></code>
	 * @param file - file to get IDOMDocument instance for 
	 * @param command - command to execute
	 */
	public static void execute(IFile file, final ICommand command) {
		IStructuredModel model = null;
		try {
			model = StructuredModelManager.getModelManager().getModelForRead(file);
			if(model instanceof IDOMModel) {
				final IDOMDocument xmlDocument = ((IDOMModel) model).getDocument();
				if(xmlDocument != null) {
					command.execute(xmlDocument);
				}
			}
		} catch (IOException e) {
			ExtensionsPlugin.getDefault().logError(e);
		} catch (CoreException e) {
			ExtensionsPlugin.getDefault().logError(e);
		} finally {
			if (model != null) {
				model.releaseFromRead();
			}
		}
	}
}
