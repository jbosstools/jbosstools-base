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
package org.jboss.tools.common.model.ui.action;

import java.util.*;
import org.eclipse.swt.*;
import org.eclipse.swt.events.*;
import org.eclipse.swt.widgets.*;
import org.eclipse.swt.graphics.*;

import org.jboss.tools.common.model.XModelException;
import org.jboss.tools.common.model.ui.Messages;
import org.jboss.tools.common.model.ui.ModelUIPlugin;
import org.jboss.tools.common.model.ui.widgets.IWidgetSettings;

public class CommandBar {
	public static final String SEPARATOR = "_SEPARATOR_"; //$NON-NLS-1$
	public static String FILL = " "; //$NON-NLS-1$
	CommandBarListener listener = null;
	Composite control = null;
	CommandBarLayout layout = new CommandBarLayout();
	ArrayList<ButtonDescriptor> buttons = new ArrayList<ButtonDescriptor>();
	String defaultCommand = null;
	boolean isMnemonicEnabled = false;
	IWidgetSettings settings = null;
	
	public void dispose() {
		if (buttons!=null) buttons.clear();
		buttons = null;
		listener = null;
		if(control != null) {
			if(!control.isDisposed()) control.dispose();
			control = null;
		}
	}
	
	public void addCommandBarListener(CommandBarListener listener) {
		this.listener = listener;
	}
	
	public CommandBarLayout getLayout() {
		return layout;
	}
	
	public void setLayout(CommandBarLayout layout) {
		this.layout = layout;
	}
	
	public void setWidgetSettings(IWidgetSettings settings) {
		this.settings = settings;
	}
	
	public Control createControl(Composite parent) {
		if (layout.asToolBar) {
			control = new ToolBar(parent, SWT.FLAT | layout.direction);
		} else {
			control = new Composite(parent, SWT.NONE);
			//control.setBackground(parent.getBackground());
			control.setBackgroundMode(SWT.INHERIT_DEFAULT);
			control.setLayout(layout);
		}
		update();
		return control;
	}
	
	public Control getControl() {
		return control;
	}
	
	/**
	 * 
	 * @param commands (translatable)
	 */
	public void setCommands(String[] commands) {
		ButtonDescriptor d = null;
		for (int i = 0; i < commands.length; i++) {
			if(buttons.size() > i) {
				d = (ButtonDescriptor)buttons.get(i); 
			} else {
				d = new ButtonDescriptor();
				buttons.add(d);
			}
			d.command = commands[i];
			// bugfix:Issue # 6167. Try find default button by name
			if (Messages.CommandBar_OK.equalsIgnoreCase(commands[i]) || 
					Messages.CommandBar_Finish.equalsIgnoreCase(commands[i]) || 
					Messages.CommandBar_Next.equalsIgnoreCase(commands[i])|| 
					Messages.CommandBar_NextArrow.equalsIgnoreCase(commands[i]) || 
					Messages.CommandBar_Run.equalsIgnoreCase(commands[i])) {
				this.defaultCommand = commands[i];
			}	
		}
		for (int i = buttons.size() - 1; i >= commands.length; i--) {
			d = (ButtonDescriptor)buttons.remove(i);
			d.dispose();
		}
	}
	
	public void setMnemonicEnabled(boolean b) {
		if(isMnemonicEnabled == b) return;
		isMnemonicEnabled = b;
		update();
	}
	
	public void setImage(String command, Image image) {
		ButtonDescriptor d = getButtonForCommand(command);
		if(d != null) d.image = image;
	}
	
	public void setEnabled(String command, boolean enabled) {
		ButtonDescriptor d = getButtonForCommand(command);
		if(d != null) d.setEnabled(enabled); 
	}
	
	public void disable() {
		for (int i = 0; i < buttons.size(); i++) {
			ButtonDescriptor d = getButtonAt(i);
			d.setEnabled(false);
		}		
	}
	
	public boolean isEnabled(String command) {
		ButtonDescriptor d = getButtonForCommand(command);
		return (d != null && d.enabled); 		
	}
	
	public void rename(String oldCommand, String newCommand) {
		ButtonDescriptor d = getButtonForCommand(oldCommand);
		if(d != null) d.rename(newCommand);		
	}
	
	public void setDefaultCommand(String command) {
		this.defaultCommand = command;
		update();
	}
	
	ButtonDescriptor getButtonForCommand(String command) {
		for (int i = 0; i < buttons.size(); i++) {
			ButtonDescriptor d = getButtonAt(i);
			if(command.equals(d.command)) return d;
		}
		return null;
	}
	
	public void update() {
		for (int i = 0; i < buttons.size(); i++) getButtonAt(i).update(); 
	}
	
	ButtonDescriptor getButtonAt(int i) {
		return (i < 0 || i >= buttons.size()) ? null : (ButtonDescriptor)buttons.get(i);
	}
	
	class ButtonDescriptor implements SelectionListener {		
		Widget button;
		String command;
		Image image = null;
		boolean enabled = true;
		
		public void createControl(Composite parent) {
			if(button != null) button.dispose();
			if (!SEPARATOR.equals(command)) {
				if (layout.asToolBar) {
					button = new ToolItem((ToolBar)parent, SWT.PUSH);
					((ToolItem)button).addSelectionListener(this);
				} else {
					int style = (settings == null) ? SWT.PUSH : settings.getStyle("Button.Style"); //$NON-NLS-1$
					if (style == SWT.DEFAULT) style = SWT.NONE;
					if (style == 0) style = SWT.PUSH;
					button = new Button(parent, style);
					if(settings != null) {
						Color bg = settings.getColor("Button.Background"); //$NON-NLS-1$
						Color fg = settings.getColor("Button.Foreground"); //$NON-NLS-1$
						Font font = settings.getFont("Button.Font"); //$NON-NLS-1$
						((Control)button).setBackground(bg);
						((Control)button).setForeground(fg);
						((Control)button).setFont(font);
					}
					((Button)button).addSelectionListener(this);
				}
				_update();
			} else {
				if (layout.asToolBar) {
					button = new ToolItem((ToolBar)parent, SWT.SEPARATOR);
				}
			}
		}
				
		public void update() {
			if(button == null || button.isDisposed()) {
				if(control != null && !control.isDisposed()) {
					createControl(control);
				}
				return;
			} else _update(); 
		}
		
		protected void _update() {
			_updateCommand();
			_updateEnabled();
			if (image != null) {
				if (layout.asToolBar) {
					((ToolItem)button).setImage(image);
				} else {
					((Button)button).setImage(image);
				}
			}
			_updateDefault();
		}
		
		void _updateEnabled() {
			if (button != null && !button.isDisposed()) {
				if (layout.asToolBar) {
					((ToolItem)button).setEnabled(enabled);
				} else {
					((Button)button).setEnabled(enabled);
				}
			}
		}
		
		void _updateCommand() {
			if(button == null || button.isDisposed()) return;
			if(layout.iconsOnly) {
				if (layout.asToolBar) {
					((ToolItem)button).setToolTipText(getText(false));
					((ToolItem)button).setText(""); //$NON-NLS-1$
				} else {
					((Button)button).setToolTipText(getText(false));
					((Button)button).setText(""); //$NON-NLS-1$
				}
				if(image != null) {
					if (layout.asToolBar) {
						((ToolItem)button).setImage(image);
					} else {
						((Button)button).setImage(image);
					}
				} 
			} else {
				if (layout.asToolBar) {
					((ToolItem)button).setToolTipText(null);
					((ToolItem)button).setText(getText(isMnemonicEnabled));
				} else {
					((Button)button).setToolTipText(null);
					((Button)button).setText(getText(isMnemonicEnabled));
				}
			}
		}
		
		String getText(boolean withMnemonic) {
			if(withMnemonic) return command;
			int i = command.indexOf('&');
			return (i < 0) ? command : command.substring(0, i) + command.substring(i + 1); 
		}
		
		void _updateDefault() {
			if (command.equalsIgnoreCase(defaultCommand) && button != null && !button.isDisposed()) { 
				if (!layout.asToolBar) {
					((Button)button).getShell().setDefaultButton((Button)button);
				}
			}
		}
		
		public void setEnabled(boolean enabled) {
			if(this.enabled == enabled) return;
			this.enabled = enabled;
			_updateEnabled();
			
		}
		
		public void rename(String newCommand) {
			if(newCommand.equals(command)) return;
			this.command = newCommand;
			_updateCommand();
		}
		
		public void setDefault() {
			_updateDefault();
		}
		
		public void widgetSelected(SelectionEvent e) {
			if(listener != null) {
				try {
					listener.action(command);
				} catch (XModelException e1) {
					ModelUIPlugin.getPluginLog().logError(e1);
				}
			}
		}

		public void widgetDefaultSelected(SelectionEvent e) {}
		
		public void dispose() {
			if(button == null || button.isDisposed()) return;
			button.dispose();
			button = null;
		}
	}
	
}
