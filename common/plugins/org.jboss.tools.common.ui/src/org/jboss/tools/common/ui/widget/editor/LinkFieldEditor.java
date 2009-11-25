package org.jboss.tools.common.ui.widget.editor;

import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.jboss.tools.common.ui.CommonUIMessages;
import org.jboss.tools.common.ui.widget.editor.ButtonFieldEditor.ButtonPressedAction;
import org.jboss.tools.common.ui.widget.field.LinkField;

public class LinkFieldEditor extends BaseFieldEditor {
	LinkField link;

	private ButtonPressedAction buttonAction = new ButtonPressedAction(CommonUIMessages.BUTTON_FIELD_EDITOR_BROWSE) {
		@Override
		public void run() {
			throw new IllegalStateException(CommonUIMessages.BUTTON_FIELD_EDITOR_NOT_IMPLEMENTED_YET);
		}
	};
	
	public LinkFieldEditor(String name, String label) {
		super(name, label, new Object());
	}
	
	public LinkFieldEditor(String name, ButtonPressedAction action, Object defaultValue) {
		super(name, action.getText(), defaultValue);
		buttonAction = action;
		buttonAction.setFieldEditor(this);
	}
	
	@Override
	public void doFillIntoGrid(Object parent) {
	}

	@Override
	public Object[] getEditorControls() {
		return new Control[]{link.getControl()};
	}

	@Override
	public boolean isEditable() {
		return false;
	}

	public void save(Object object) {
	}

	@Override
	public void setEditable(boolean ediatble) {
	}

	@Override
	public Object[] getEditorControls(Object composite) {
		if(link == null && composite != null) {
			link = new LinkField((Composite)composite, buttonAction);
		}
		return new Control[]{link.getControl()};
	}

	public ButtonPressedAction getButtonaction() {
		return buttonAction;
	}

	@Override
	public int getNumberOfControls() {
		return 1;
	}

}
