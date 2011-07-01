/*******************************************************************************
 * Copyright (c) 2000, 2011 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *     Exadel, Inc.
 *     Red Hat, Inc. 
 *******************************************************************************/
package org.jboss.tools.common.model.ui.actions.xpl;

import java.util.Iterator;
import java.util.ResourceBundle;

import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.ITextOperationTarget;
import org.eclipse.jface.text.Position;
import org.eclipse.jface.text.source.Annotation;
import org.eclipse.jface.text.source.IAnnotationAccessExtension;
import org.eclipse.jface.text.source.ISourceViewer;
import org.eclipse.jface.text.source.IVerticalRulerInfo;
import org.eclipse.swt.widgets.Event;
import org.eclipse.ui.texteditor.AbstractMarkerAnnotationModel;
import org.eclipse.ui.texteditor.ITextEditor;
import org.eclipse.ui.texteditor.ITextEditorExtension;
import org.eclipse.ui.texteditor.SelectMarkerRulerAction;

public class SelectAnnotationRulerAction  extends SelectMarkerRulerAction {

	private ITextEditor fTextEditor;
	private Position fPosition;
	private boolean fHasCorrection;
	private ResourceBundle fBundle;

	public SelectAnnotationRulerAction(ResourceBundle bundle, String prefix, ITextEditor editor, IVerticalRulerInfo ruler) {
		super(bundle, prefix, editor, ruler);
		fBundle= bundle;
		fTextEditor= editor;
	}

	@Override
	public void run() {
		runWithEvent(null);
	}

	@Override
	public void runWithEvent(Event event) {

		if (fHasCorrection) {
			ITextOperationTarget operation= (ITextOperationTarget) fTextEditor.getAdapter(ITextOperationTarget.class);
			final int opCode= ISourceViewer.QUICK_ASSIST;
			if (operation != null && operation.canDoOperation(opCode)) {
				fTextEditor.selectAndReveal(fPosition.getOffset(), fPosition.getLength());
				operation.doOperation(opCode);
			}
			return;
		}

		super.run();
	}

	@Override
	public void update() {
		findAnnotation();
		setEnabled(true);
		if (fHasCorrection) {
			initialize(fBundle, "SelectAnnotationRulerAction.QuickFix."); //$NON-NLS-1$
			return;
		}

		initialize(fBundle, "SelectAnnotationRulerAction.GotoAnnotation."); //$NON-NLS-1$;
		super.update();
	}

	private void findAnnotation() {
		fPosition= null;
		fHasCorrection= false;

		AbstractMarkerAnnotationModel model= getAnnotationModel();
		IAnnotationAccessExtension annotationAccess= getAnnotationAccessExtension();

		IDocument document= getDocument();
		if (model == null)
			return ;

		Iterator<Annotation> iter= model.getAnnotationIterator();
		int layer= Integer.MIN_VALUE;

		while (iter.hasNext()) {
			Annotation annotation= iter.next();
			if (annotation.isMarkedDeleted())
				continue;

			int annotationLayer= layer;
			if (annotationAccess != null) {
				annotationLayer= annotationAccess.getLayer(annotation);
				if (annotationLayer < layer)
					continue;
			}

			Position position= model.getPosition(annotation);
			if (!includesRulerLine(position, document))
				continue;

			boolean isReadOnly= fTextEditor instanceof ITextEditorExtension && ((ITextEditorExtension)fTextEditor).isEditorInputReadOnly();
			if (!isReadOnly) {
				fPosition= position;
				fHasCorrection= true;
				layer= annotationLayer;
				continue;
			} 
		}
	}

}
