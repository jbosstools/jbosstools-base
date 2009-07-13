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
package org.jboss.tools.common.model.ui.attribute.editor;

import java.text.MessageFormat;
import org.jboss.tools.common.model.ui.IValueChangeListener;
import org.jboss.tools.common.model.ui.IValueProvider;
import org.jboss.tools.common.model.ui.attribute.AttributeContentProposalProviderFactory;
import org.jboss.tools.common.model.ui.attribute.adapter.DefaultValueAdapter;
import org.eclipse.jdt.internal.ui.refactoring.contentassist.ControlContentAssistHelper;
import org.eclipse.jface.fieldassist.ContentProposalAdapter;
import org.eclipse.jface.fieldassist.IContentProposalListener2;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.resource.ImageRegistry;
import org.eclipse.jface.resource.JFaceResources;
import org.eclipse.jface.text.contentassist.IContentAssistProcessor;
import org.eclipse.jface.viewers.DialogCellEditor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.events.FocusListener;
import org.eclipse.swt.events.KeyAdapter;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Text;

import org.jboss.tools.common.meta.key.WizardKeys;
import org.jboss.tools.common.model.ui.ModelUIPlugin;

public class DialogCellEditorEx extends DialogCellEditor { //implements IValueEditor {
	protected Text text;
	protected String textValue = null;
	private Button button;
	private static final int defaultStyle = SWT.NONE;
	private boolean skipDeactivate = Boolean.FALSE.booleanValue();

	static {
		ImageRegistry reg = JFaceResources.getImageRegistry();
		reg.put(CELL_EDITOR_IMG_DOTS_BUTTON, ImageDescriptor.createFromFile(DialogCellEditor.class, "images/dots_button.gif"));//$NON-NLS-1$
	}
	
	// IValueEditor
	protected IValueChangeListener valueChangeListener;
	protected IValueProvider valueProvider;
		
	protected PropertyEditor propertyEditor;
	protected PropertyEditorDialog dialog;
	
	public DialogCellEditorEx() {
		setStyle(defaultStyle);
	}

	// constructors
	
	public DialogCellEditorEx(Composite parent) {
		this(parent, defaultStyle);
	}
	
	public DialogCellEditorEx(Composite parent, int style) {
		super(parent, style);
	}

	protected Control getTextControl() {
		return text;
	}
	
	protected Button getButtonControl() {
		return button;
	}
	
	// methods
	
	protected Button createButton(Composite parent) {
		Button result = new Button(parent, SWT.DOWN);
		result.addFocusListener(new FocusListener() {
			public void focusGained(FocusEvent e) {
			}
			public void focusLost(FocusEvent e) {
				DialogCellEditorEx.this.focusLost();
			}
		});
		result.setText("..."); //$NON-NLS-1$
		return button = result;
	}
	
	protected Control createControl(Composite parent) {
		return (Composite)super.createControl(parent);
	}

	protected void doSetValue(Object value) {
		super.doSetValue(value);
		super.fireEditorValueChanged(true,false);
	}

	protected Object openDialogBox(Control cellEditorWindow) {
		if (propertyEditor!=null) {
			Object oldValue = ((IValueProvider)propertyEditor.getAdapter(IValueProvider.class)).getValue();
			DefaultValueAdapter a = (DefaultValueAdapter)propertyEditor.getInput();
			boolean auto = a.isAutoStore();
			a.setAutoStore(false);
			propertyEditor.setValue(getValue());
			if(propertyEditor.callsExternal()) {
				externalEditing = true;
				Object s = propertyEditor.callExternal(cellEditorWindow.getShell());
				if(s != null && s.equals(oldValue)) s = null;
				if(s != null) { 
					propertyEditor.setValue(s);
				} else {
					if(editable) text.forceFocus();
				}
				a.setAutoStore(auto);
				externalEditing = false;
				return s;
			}
			dialog = new PropertyEditorDialog(ModelUIPlugin.getDefault().getWorkbench().getActiveWorkbenchWindow().getShell(),propertyEditor);
			dialog.create();
			String title = MessageFormat.format("Edit {0}", WizardKeys.getAttributeDisplayName(a.getAttribute(), true));
			dialog.getShell().setText(title);
			externalEditing = true;
			ExtendedFieldEditor fieldEditor = propertyEditor.getFieldEditor(null);
			int i = dialog.open();
			if(i != 0) {
				propertyEditor.setValue(oldValue);
				a.setAutoStore(auto);
				if(editable) text.forceFocus();
				this.externalEditing = false;
				return null;
			}
			//propertyEditor.setValue(this.doGetValue());
			fieldEditor.store(); 
			a.setAutoStore(auto);
			this.externalEditing = false;
			return ((IValueProvider)propertyEditor.getAdapter(IValueProvider.class)).getValue();
		}
		return null;
	}

	public void setPropertyEditor(PropertyEditor editor) {
		propertyEditor = editor;
		if(editor != null) {
			addContentAssist(text);
//			IContentAssistProcessor processor = (IContentAssistProcessor)editor.getAdapter(IContentAssistProcessor.class);
//			if(processor != null) {
//				ControlContentAssistHelper.createTextContentAssistant(text, processor);
//			}
		}
	}

	// IValueEditor
	//public void setValueChangeListener(IValueChangeListener valueChangeListener) {
	//	this.valueChangeListener = valueChangeListener;
	//}
	public void setValueProvider(IValueProvider valueProvider) {
		this.valueProvider = valueProvider;
		setValue(valueProvider.getValue());
	}
	
	protected Control createContents(Composite cell) {
		text = new Text(cell, SWT.LEFT);
		text.setFont(cell.getFont());
		text.setBackground(cell.getBackground());
		text.addKeyListener(new KeyAdapter() {
			public void keyPressed(KeyEvent e) {
				if ((byte)e.character == (byte)13) { // Enter
					if(popup.isPopupOpened) return;
					fireApplyEditorValue();
					fireCancelEditor();
				}
			}
			public void keyReleased(KeyEvent e) {
				if (e.character == '\u001b') { // Escape
					fireCancelEditor();
				}
			}
		});
		text.addModifyListener(new ModifyListener() {
			public void modifyText(ModifyEvent e) {
				if(modifyLock > 0) return;
				modifyLock++;
				doSetValue(textValue != null ? textValue : text.getText());
				modifyLock--;
			}
		});
		text.addFocusListener(new FocusListener() {
			public void focusGained(FocusEvent e) {
			}
			public void focusLost(FocusEvent e) {
				DialogCellEditorEx.this.focusLost();
			}
		});

		return text;
	}
	
	protected void focusLost() {
		Control focusControl = Display.getCurrent().getFocusControl();
		if (focusControl!=null && (focusControl.equals(button) || focusControl.equals(text))) return;
		if(skipDeactivate) return;
		if(!externalEditing) {
			String v = (textValue != null) ? textValue : text.getText();
			modifyLock++;
			super.doSetValue(v);
			modifyLock--;
			super.focusLost();
		}
	}
	
	protected boolean externalEditing = false;
	int modifyLock = 0;
	boolean editable = true;

	protected void updateContents(Object value) {
		if(modifyLock > 0) return;
		if (text == null) return;		
		String txt = ""; //$NON-NLS-1$
		if (value != null) txt = value.toString();
		setTextEditable(!(txt.indexOf('\n') >= 0 || txt.indexOf('\r') >= 0));
		if(!editable) {
			textValue = txt;
			txt = txt.replace('\n', ' ').replace('\t', ' ').replace('\r', ' ');  
		}
		modifyLock++;
		text.setText(txt);
		super.doSetValue(value);
		modifyLock--;
		doSetFocus();
	}
	
	void setTextEditable(boolean b) {
		if(editable == b) return;
		editable = b;
		text.setEditable(b);
		int c = (b) ? SWT.COLOR_BLACK : SWT.COLOR_GRAY;
		text.setForeground(Display.getDefault().getSystemColor(c));
	}

	protected void doSetFocus() {
		if(editable && !externalEditing) {
			text.forceFocus();
			text.setSelection(0, text.getText().length());
		} 
	}
	
	protected void fireApplyEditorValue() {
		super.fireApplyEditorValue();

		if (propertyEditor!=null) {
			if (propertyEditor.getInput() instanceof DefaultValueAdapter) {
				if(((DefaultValueAdapter)propertyEditor.getInput()).isAutoStore()) fireCancelEditor();
			}
		}
		else if(text != null && !text.isDisposed() && editable) text.forceFocus();
	}

	protected void addContentAssist(Text text) {
		if(propertyEditor != null && propertyEditor.getInput() instanceof DefaultValueAdapter) {
			DefaultValueAdapter valueAdapter = (DefaultValueAdapter)propertyEditor.getInput();
			AttributeContentProposalProviderFactory.registerContentAssist(valueAdapter, text, popup);
		}
	}

	protected CPL2 popup = new CPL2();
	
	class CPL2 implements IContentProposalListener2 {
		boolean isPopupOpened = false;

		public void proposalPopupClosed(ContentProposalAdapter adapter) {
			isPopupOpened = false;			
		}

		public void proposalPopupOpened(ContentProposalAdapter adapter) {
			isPopupOpened = true;			
		}
	}

	protected void fireCancelEditor() {
		skipDeactivate = Boolean.TRUE.booleanValue();
		super.fireCancelEditor();
		skipDeactivate = Boolean.FALSE.booleanValue();
		deactivate();
	}
	
	public void activate() {
		super.activate();
		skipDeactivate = false;
	}

	public void deactivate() {
		if (!this.skipDeactivate) {
			skipDeactivate = true;
			super.deactivate();
		}
	}
	
}
