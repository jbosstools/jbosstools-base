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
package org.jboss.tools.common.model.ui.texteditors.propertyeditor;

import java.util.Properties;

import org.jboss.tools.common.model.ui.editor.IModelObjectEditorInput;
import org.jboss.tools.common.model.ui.views.palette.PaletteInsertHelper;

import org.eclipse.core.resources.IResource;
import org.jboss.tools.common.model.ui.texteditors.dnd.TextEditorDrop;
import org.jboss.tools.common.model.ui.texteditors.dnd.TextEditorDropProvider;
import org.eclipse.jdt.internal.ui.JavaPlugin;
import org.eclipse.jdt.internal.ui.propertiesfileeditor.IPropertiesFilePartitions;
import org.eclipse.jdt.internal.ui.propertiesfileeditor.PropertiesFileEditor;
import org.eclipse.jdt.internal.ui.propertiesfileeditor.PropertiesFileSourceViewerConfiguration;
import org.eclipse.jdt.ui.text.JavaTextTools;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.text.DocumentEvent;
import org.eclipse.jface.text.IDocumentListener;
import org.eclipse.jface.text.source.ISourceViewer;
import org.eclipse.ui.IFileEditorInput;
import org.eclipse.ui.editors.text.TextFileDocumentProvider;
import org.eclipse.ui.texteditor.IDocumentProvider;

import org.jboss.tools.common.model.XModelObject;

/**
 * @author Jeremy
 *
 * To change the template for this generated type comment go to
 * Window>Preferences>Java>Code Generation>Code and Comments
 */
public class PropertiesTextEditorStub extends PropertiesFileEditor implements IDocumentListener {
	protected TextEditorDrop dnd = new TextEditorDrop();

	public PropertiesTextEditorStub() {
		dnd.setTextEditorDropProvider(new TextEditorDropProviderImpl());
	}

	public XModelObject getModelObject() {
		if (getEditorInput() instanceof IModelObjectEditorInput) {
			return ((IModelObjectEditorInput)getEditorInput()).getXModelObject();
		}
		return null;
	}

	class TextEditorDropProviderImpl implements TextEditorDropProvider {

		public ISourceViewer getSourceViewer() {
			return PropertiesTextEditorStub.this.getSourceViewer();
		}

		public XModelObject getModelObject() {
			return PropertiesTextEditorStub.this.getModelObject();
		}
	
		public void insert(Properties p) {
			PaletteInsertHelper.getInstance().insertIntoEditor(getSourceViewer(), p);
		}

	}

	public void save() {}

	public void documentAboutToBeChanged(DocumentEvent event) {}

	public void documentChanged(DocumentEvent event) {
	}

	IDocumentProvider provider = null;
	public IDocumentProvider getDocumentProvider() {
		if (provider == null) {
			provider = new PDP();
			setDocumentProvider(provider);
		}
		return provider;
	}
	
	class PDP extends TextFileDocumentProvider {
		public boolean isSynchronized(Object element) {
			if (element instanceof IFileEditorInput) {
				IFileEditorInput input = (IFileEditorInput) element;
				IResource resource = input.getFile();
				return resource != null && resource.isSynchronized(IResource.DEPTH_ZERO);
//				return false;
			}
			return super.isSynchronized(element);
		}
		
	}

	protected void initializeEditor() {
		super.initializeEditor();
		IPreferenceStore store= JavaPlugin.getDefault().getCombinedPreferenceStore();
		JavaTextTools textTools= JavaPlugin.getDefault().getJavaTextTools();
		setSourceViewerConfiguration(new PropertiesSourceViewerConfiguration(textTools.getColorManager(), store, this, IPropertiesFilePartitions.PROPERTIES_FILE_PARTITIONING));
	}
}
