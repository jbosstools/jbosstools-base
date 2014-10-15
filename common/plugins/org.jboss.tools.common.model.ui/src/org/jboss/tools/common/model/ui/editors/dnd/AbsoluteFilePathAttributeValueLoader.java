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

import java.util.Properties;

import org.eclipse.core.resources.IFile;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IFileEditorInput;
import org.jboss.tools.common.model.XModelBuffer;
import org.jboss.tools.common.model.XModelException;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.ui.dnd.DnDUtil;
import org.jboss.tools.common.model.util.EclipseResourceUtil;
import org.jboss.tools.common.web.WebUtils;

public class AbsoluteFilePathAttributeValueLoader implements IAttributeValueLoader {
	String fPathAttributeName,
		fWidthAttributeName,
		fHeightAttributeName;
	public AbsoluteFilePathAttributeValueLoader(
			String pathAttributeName,
			String widthAttributeName,
			String heightAttributeName
	) {
		fPathAttributeName = pathAttributeName;
		// Not used yet
		fWidthAttributeName = widthAttributeName;
		fHeightAttributeName = heightAttributeName;		
	}
	
	public void fillTagAttributes(IDropWizardModel model) {
		IFile file = DropUtils.getResourceForMimeData(model.getDropData());
		if(file == null) return;
		IEditorInput editorInput = model.getDropData().getEditorInput();
		IFile context = null;
		if(editorInput instanceof IFileEditorInput) {
			context = ((IFileEditorInput)editorInput).getFile();
		}

		String v = (context == null) ? null : dropFileToFile(file, context, model);
		if(v == null) {
			v = getPath(context, file);
		}

		model.setAttributeValue(fPathAttributeName, v);
		if(model instanceof DefaultDropWizardModel) {
			((DefaultDropWizardModel)model).setPreferable(fPathAttributeName);
		}
	}

	protected String getPath(IFile context, IFile resource) {
		return WebUtils.getWebPath(null, resource);
	}

	/**
	 * Computes text to be inserted
	 */	
	private String dropFileToFile(IFile source, IFile target, IDropWizardModel model) {
		XModelObject t = EclipseResourceUtil.getObjectByResource(target);
		XModelObject s = EclipseResourceUtil.getObjectByResource(source);
		if(t == null || s == null) return null;
		XModelBuffer b = s.getModel().getModelBuffer();
		XModelObject c = b.source();
		b.clear();
		b.addSource(s);
		try {
			if(!DnDUtil.isPasteEnabled(t)) return null;
			Properties p = new Properties();
			String tagName = model.getTagProposal().getName();
			p.setProperty("isDrop", "true"); //$NON-NLS-1$ //$NON-NLS-2$
			if(tagName != null) p.setProperty("context:tagName", tagName); //$NON-NLS-1$
			
			DnDUtil.paste(t, p);
			return p.getProperty("start text"); //$NON-NLS-1$
		} catch (XModelException e) {
			return null;
		} finally {
			b.clear();
			if(c != null) b.addSource(c);
		}
	}
}
