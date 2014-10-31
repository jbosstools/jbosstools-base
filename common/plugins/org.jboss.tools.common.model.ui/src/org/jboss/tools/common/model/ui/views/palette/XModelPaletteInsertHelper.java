/******************************************************************************* 
 * Copyright (c) 2014 Red Hat, Inc. 
 * Distributed under license by Red Hat, Inc. All rights reserved. 
 * This program is made available under the terms of the 
 * Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html 
 * 
 * Contributors: 
 * Red Hat, Inc. - initial API and implementation 
 ******************************************************************************/ 
package org.jboss.tools.common.model.ui.views.palette;

import java.util.Properties;

import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.ITextSelection;
import org.eclipse.jface.text.TextSelection;
import org.eclipse.jface.text.source.ISourceViewer;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.texteditor.ITextEditor;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.options.SharableConstants;
import org.jboss.tools.common.model.ui.editor.IModelObjectEditorInput;

public class XModelPaletteInsertHelper extends PaletteInsertHelper {
	static XModelPaletteInsertHelper instance = new XModelPaletteInsertHelper();

    public static XModelPaletteInsertHelper getInstance() {
    	return instance;
    }
    
	protected boolean isEditable(IEditorInput input) {
		if(input instanceof IModelObjectEditorInput) {
			XModelObject o = ((IModelObjectEditorInput)input).getXModelObject();
			if(o != null && !o.isObjectEditable()) return false;
		}
		return super.isEditable(input);
	}
	
	public int correctOffset(IDocument document, int offset, String paletteItemPath){
		ITextSelection selection = correctSelection(document, new TextSelection(document, offset, 0), paletteItemPath);
		return selection.getOffset();
	}
	
	public ITextSelection correctSelection(IDocument document, ITextSelection selection, String paletteItemPath){
		 IPositionCorrector corrector = getCorrector(paletteItemPath);
		 return correctSelection(document, selection, corrector);
	}
	
	public void insertIntoEditor(final ISourceViewer v, Properties p){
		String paletteItemPath = p.getProperty(SharableConstants.PALETTE_PATH);
		IPositionCorrector corrector = getCorrector(paletteItemPath);
		insertIntoEditor(v, p, corrector);
	}
	
	public void insertIntoEditor(ITextEditor editor, Properties p) {
		String paletteItemPath = p.getProperty(SharableConstants.PALETTE_PATH);
		IPositionCorrector corrector = getCorrector(paletteItemPath);
		insertIntoEditor(editor, p, corrector);
	}
	
	private IPositionCorrector getCorrector(String paletteItemPath){
		if(paletteItemPath == null){
			return null;
		}
		return PaletteInsertManager.getInstance().createCorrectorInstance(paletteItemPath);
	}
}
