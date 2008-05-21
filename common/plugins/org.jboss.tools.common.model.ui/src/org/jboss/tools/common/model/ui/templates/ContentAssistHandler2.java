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
package org.jboss.tools.common.model.ui.templates;

import org.eclipse.swt.events.*;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.jface.contentassist.*;
import org.eclipse.jface.text.contentassist.IContentAssistant;
import org.eclipse.jface.viewers.*;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.handlers.IHandlerActivation;
import org.eclipse.ui.handlers.IHandlerService;
import org.eclipse.ui.texteditor.ITextEditorActionDefinitionIds;

// see org.eclipse.ui.contentassist.ContentAssistHandler

public class ContentAssistHandler2 {
	private Control control;
	private AbstractControlContentAssistSubjectAdapter adapter;
	private IContentAssistant assistant;
	private FocusListener focusListener;
	private ModifyListener modifyListener;
	private IHandlerActivation activation;
	private ILabelProvider cueLabel;

	public static ContentAssistHandler2 createHandlerForCombo(Combo combo, SubjectControlContentAssistant contentAssistant) {
		return new ContentAssistHandler2(combo, new ComboContentAssistSubjectAdapter(combo), contentAssistant, null);
	}
		
	public static ContentAssistHandler2 createHandlerForText(Text text, SubjectControlContentAssistant contentAssistant, ILabelProvider cueLabel) {
		ContentAssistHandler2 cah = new ContentAssistHandler2(text, new TextContentAssistSubjectAdapter(text), contentAssistant, cueLabel);
		return cah;
	}
	
	private ContentAssistHandler2(
			Control control,
			AbstractControlContentAssistSubjectAdapter subjectAdapter,
			SubjectControlContentAssistant contentAssistant,
			ILabelProvider cueLabel) {
		this.control = control;
		assistant = contentAssistant;
		adapter = subjectAdapter;
		this.cueLabel = cueLabel;
		setEnabled(true);
		control.addDisposeListener(new DisposeListener() {
			public void widgetDisposed(DisposeEvent e) {
				setEnabled(false);
			}
		});
	}
	
	public boolean isEnabled() {
		return focusListener != null;
	}
	
	public void setEnabled(boolean enable) {
		if (enable == isEnabled()) return;
		if (enable)	enable(); else disable();
	}
	
	private void enable() {
		if (!control.isDisposed()) {
			if(assistant instanceof SubjectControlContentAssistant) {
				((SubjectControlContentAssistant)assistant).install(adapter);
			}
			installCueLabelProvider();
			installFocusListener();
			if (control.isFocusControl())
				activate();
		}
	}
	
	private void disable() {
		if (!control.isDisposed()) {
			assistant.uninstall();
			adapter.setContentAssistCueProvider(null);
			control.removeFocusListener(focusListener);
			focusListener= null;
			if (activation != null)
				deactivate();
		}
	}
	
	private void installCueLabelProvider() {
		adapter.setContentAssistCueProvider(cueLabel);
	}
		private void installFocusListener() {
		focusListener = new FocusListener() {
			public void focusGained(final FocusEvent e) {
				if(control instanceof Text && !control.isDisposed()) {
					((Text)control).addModifyListener(getModifyListener());
				}
				activate();
			}
			public void focusLost(FocusEvent e) {
				if (activation != null)
					if(control instanceof Text && modifyListener != null && !control.isDisposed()) {
						((Text)control).removeModifyListener(modifyListener);
					}
					deactivate();
			}
		};
		control.addFocusListener(focusListener);
	}
	
	ModifyListener getModifyListener() {
		if(modifyListener == null) {
			modifyListener = new ModifyListenerImpl();
		}
		return modifyListener;
	}
	
	class ModifyListenerImpl implements ModifyListener {
		public void modifyText(ModifyEvent e) {
			if(control == null || control.isDisposed()) return;
			if(control.isFocusControl()) {
				Control c = control.getParent();
				while (c != null) {
					c.redraw();
					if(c instanceof Shell) break; else c = c.getParent();
				}
			}
		}			
	}
	
	private void activate() {
		IHandlerService hs = (IHandlerService)PlatformUI.getWorkbench().getAdapter(IHandlerService.class);
		if (hs == null) return;			
		activation = hs.activateHandler(ITextEditorActionDefinitionIds.CONTENT_ASSIST_PROPOSALS, new H());
	}

	class H extends AbstractHandler {
		public Object execute(ExecutionEvent event) throws ExecutionException {
			if (ContentAssistHandler2.this.isEnabled()) // don't call AbstractHandler#isEnabled()!
				assistant.showPossibleCompletions();
			return null;
		}
	}
	private void deactivate() {
		IHandlerService hs = (IHandlerService)PlatformUI.getWorkbench().getAdapter(IHandlerService.class);
		if (hs != null) hs.deactivateHandler(activation);
		activation = null;
	}

}
