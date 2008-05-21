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
package org.jboss.tools.common.model.ui.editors.dnd;

import java.io.File;
import java.util.Properties;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;

import org.jboss.tools.common.meta.action.XActionInvoker;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.util.EclipseResourceUtil;
import org.jboss.tools.common.model.ui.dnd.ModelTransfer;
import org.jboss.tools.common.model.ui.views.palette.PaletteInsertHelper;

/**
 * 
 * @author eskimo
 */

// TODO - Eskimo - Think about inserting different type of tags
// - HTML
// - XML(XHTML,JSP)

public class FileDropCommand extends DefaultDropCommand {

	IElementGenerator generator;

	public void run(IProgressMonitor monitor) throws CoreException {

		generator = ElementGeneratorFactory.getInstance().getElementGenerator(getDefaultModel().getTagProposal().getUri());
		generator.setDataModel(getDefaultModel());

		Properties properties = new Properties();
		properties.put(PaletteInsertHelper.PROPOPERTY_TAG_NAME,getDefaultModel().getTagProposal().getName());
		properties.put(PaletteInsertHelper.PROPOPERTY_START_TEXT, generateStartText());
		properties.put(PaletteInsertHelper.PROPOPERTY_END_TEXT, generateEndText());
		properties.put(PaletteInsertHelper.PROPOPERTY_REFORMAT_BODY, getReformatBodyProperty());
		properties.put(PaletteInsertHelper.PROPOPERTY_TAGLIBRARY_URI,getDefaultModel().getTagProposal().getUri());
		properties.put(PaletteInsertHelper.PROPOPERTY_TAGLIBRARY_VERSION,getDefaultModel().getTagProposal().getLibraryVersion());
		properties.put(PaletteInsertHelper.PROPOPERTY_DEFAULT_PREFIX,getDefaultModel().getTagProposal().getPrefix());
		properties.put(PaletteInsertHelper.PROPOPERTY_SELECTION_PROVIDER, getDefaultModel().getDropData().getSelectionProvider());
		properties.put(PaletteInsertHelper.PROPOPERTY_ADD_TAGLIB, "true");
		addCustomProperties(properties);
		PaletteInsertHelper.insertIntoEditor(
				getDefaultModel().getDropData().getSourceViewer(),
				properties
		);
	}

	protected void executeUnknownTag() {
		DropData data = getDefaultModel().getDropData();
		String type = data.getMimeType();
		String mime = data.getMimeData();
		/*nsITransferable.kFileMime*/
		if("application/x-moz-file".equals(type) && mime != null) {
			if(mime.startsWith("file:")) mime = mime.substring(5);
			File f = new File(mime);
			IFile file = EclipseResourceUtil.getFile(f.getAbsolutePath());
			if(file == null) return;
			XModelObject o = EclipseResourceUtil.getObjectByResource(file);
			if(o == null) return;
			XActionInvoker.invoke("CopyActions.Copy", o, null);
			data.setMimeType(ModelTransfer.MODEL);
			data.setMimeData("");
			DropCommandFactory.getInstance().getDropCommand(ModelTransfer.MODEL, tagProposalFactory).execute(data);
		}
	}

	protected void addCustomProperties(Properties properties) {}

	protected String getReformatBodyProperty() {
		return "yes";
	}

	protected IDropWizardModel createSpecificModel() {
		return new ExternalDropWizardModel(tagProposalFactory);
	}

	protected String generateStartText() {
		return generator.generateStartTag();
	}

	protected String generateEndText() {
		return generator.generateEndTag();
	}
}