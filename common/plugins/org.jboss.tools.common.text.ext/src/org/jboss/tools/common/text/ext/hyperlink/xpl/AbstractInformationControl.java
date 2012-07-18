/*******************************************************************************
 * Copyright (c) 2000, 2011 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *     Red Hat, Inc.
 *******************************************************************************/
package org.jboss.tools.common.text.ext.hyperlink.xpl;

import java.util.List;

import org.eclipse.jdt.core.IType;
import org.eclipse.jdt.internal.ui.JavaPlugin;
import org.eclipse.jdt.internal.ui.util.StringMatcher;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.dialogs.PopupDialog;
import org.eclipse.jface.text.IInformationControl;
import org.eclipse.jface.text.IInformationControlExtension;
import org.eclipse.jface.text.IInformationControlExtension2;
import org.eclipse.jface.text.hyperlink.IHyperlink;
import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.FocusListener;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.KeyListener;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.MouseAdapter;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseMoveListener;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Item;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableItem;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.IWorkbenchCommandConstants;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.commands.ActionHandler;
import org.eclipse.ui.commands.HandlerSubmission;
import org.eclipse.ui.commands.ICommand;
import org.eclipse.ui.commands.ICommandManager;
import org.eclipse.ui.commands.IKeySequenceBinding;
import org.eclipse.ui.commands.Priority;
import org.eclipse.ui.keys.KeySequence;

/**
 * Abstract class for Show hierarchy in light-weight controls.
 *
 * @since 2.1
 */
public abstract class AbstractInformationControl extends PopupDialog implements IInformationControl, IInformationControlExtension, IInformationControlExtension2, DisposeListener {

	/** The control's text widget */
	private Text fFilterText;
	/** The control's table widget */
	private TableViewer fTableViewer;
	/** The current string matcher */
	protected StringMatcher fStringMatcher;
	private ICommand fInvokingCommand;
	private KeySequence[] fInvokingCommandKeySequences;

	/**
	 * Fields that support the dialog menu
	 * @since 3.0
	 * @since 3.2 - now appended to framework menu
	 */
	private Composite fViewMenuButtonComposite;

	private IAction fShowViewMenuAction;
	private HandlerSubmission fShowViewMenuHandlerSubmission;

	/**
	 * Field for table style since it must be remembered by the instance.
	 *
	 * @since 3.2
	 */
	private int fTableStyle;

	/**
	 * The initially selected type.
	 * @since 3.5
	 */
	protected IType fInitiallySelectedType;

	/**
	 * Creates a table information control with the given shell as parent. The given
	 * styles are applied to the shell and the table widget.
	 *
	 * @param parent the parent shell
	 * @param shellStyle the additional styles for the shell
	 * @param tableStyle the additional styles for the table widget
	 * @param invokingCommandId the id of the command that invoked this control or <code>null</code>
	 * @param showStatusField <code>true</code> iff the control has a status field at the bottom
	 */
	public AbstractInformationControl(Shell parent, int shellStyle, int tableStyle, String invokingCommandId, boolean showStatusField) {
		super(parent, shellStyle, true, true, false, true, true, null, null);
		if (invokingCommandId != null) {
			ICommandManager commandManager= PlatformUI.getWorkbench().getCommandSupport().getCommandManager();
			fInvokingCommand= commandManager.getCommand(invokingCommandId);
			if (fInvokingCommand != null && !fInvokingCommand.isDefined())
				fInvokingCommand= null;
			else
				// Pre-fetch key sequence - do not change because scope will change later.
				getInvokingCommandKeySequences();
		}
		fTableStyle= tableStyle;
		// Title and status text must be set to get the title label created, so force empty values here.
		if (hasHeader())
			setTitleText(""); //$NON-NLS-1$
		setInfoText(""); //  //$NON-NLS-1$

		// Create all controls early to preserve the life cycle of the original implementation.
		create();

		// Status field text can only be computed after widgets are created.
		setInfoText(getStatusFieldText());
	}

	/**
	 * Create the main content for this information control.
	 *
	 * @param parent The parent composite
	 * @return The control representing the main content.
	 * @since 3.2
	 */
	@Override
	protected Control createDialogArea(Composite parent) {
		fTableViewer= createTableViewer(parent, fTableStyle);

		final Table table= fTableViewer.getTable();
		table.addKeyListener(new KeyListener() {
			public void keyPressed(KeyEvent e)  {
				if (e.character == 0x1B) // ESC
					dispose();
			}
			public void keyReleased(KeyEvent e) {
				// do nothing
			}
		});

		table.addSelectionListener(new SelectionListener() {
			public void widgetSelected(SelectionEvent e) {
				// do nothing
			}
			public void widgetDefaultSelected(SelectionEvent e) {
				gotoSelectedElement();
			}
		});

		table.addMouseMoveListener(new MouseMoveListener()	 {
			TableItem fLastItem= null;
			public void mouseMove(MouseEvent e) {
				if (table.equals(e.getSource())) {
					Object o= table.getItem(new Point(e.x, e.y));
					if (fLastItem == null ^ o == null) {
						table.setCursor(o == null ? null : table.getDisplay().getSystemCursor(SWT.CURSOR_HAND));
					}
					if (o instanceof TableItem) {
						Rectangle clientArea = table.getClientArea();
						if (!o.equals(fLastItem)) {
							fLastItem= (TableItem)o;
							table.setSelection(new TableItem[] { fLastItem });
						} else if (e.y - clientArea.y < table.getItemHeight() / 4) {
							// Scroll up
							Point p= table.toDisplay(e.x, e.y);
							Item item= fTableViewer.scrollUp(p.x, p.y);
							if (item instanceof TableItem) {
								fLastItem= (TableItem)item;
								table.setSelection(new TableItem[] { fLastItem });
							}
						} else if (clientArea.y + clientArea.height - e.y < table.getItemHeight() / 4) {
							// Scroll down
							Point p= table.toDisplay(e.x, e.y);
							Item item= fTableViewer.scrollDown(p.x, p.y);
							if (item instanceof TableItem) {
								fLastItem= (TableItem)item;
								table.setSelection(new TableItem[] { fLastItem });
							}
						}
					} else if (o == null) {
						fLastItem= null;
					}
				}
			}
		});

		table.addMouseListener(new MouseAdapter() {
			@Override
			public void mouseUp(MouseEvent e) {

				if (table.getSelectionCount() < 1)
					return;

				if (e.button != 1)
					return;

				if (table.equals(e.getSource())) {
					Object o= table.getItem(new Point(e.x, e.y));
					TableItem selection= table.getSelection()[0];
					if (selection.equals(o))
						gotoSelectedElement();
				}
			}
		});

		installFilter();

		addDisposeListener(this);
		return fTableViewer.getControl();
	}

	/**
	 * Creates a table information control with the given shell as parent. The given
	 * styles are applied to the shell and the table widget.
	 *
	 * @param parent the parent shell
	 * @param shellStyle the additional styles for the shell
	 * @param tableStyle the additional styles for the table widget
	 */
	public AbstractInformationControl(Shell parent, int shellStyle, int tableStyle) {
		this(parent, shellStyle, tableStyle, null, false);
	}

	protected abstract TableViewer createTableViewer(Composite parent, int style);

	/**
	 * Returns the name of the dialog settings section.
	 *
	 * @return the name of the dialog settings section
	 */
	protected abstract String getId();

	protected TableViewer getTableViewer() {
		return fTableViewer;
	}

	/**
	 * Returns <code>true</code> if the control has a header, <code>false</code> otherwise.
	 * <p>
	 * The default is to return <code>false</code>.
	 * </p>
	 *
	 * @return <code>true</code> if the control has a header
	 */
	protected boolean hasHeader() {
		// default is to have no header
		return false;
	}

	protected Text getFilterText() {
		return fFilterText;
	}

	protected Text createFilterText(Composite parent) {
		fFilterText= new Text(parent, SWT.NONE);
		Dialog.applyDialogFont(fFilterText);

		GridData data= new GridData(GridData.FILL_HORIZONTAL);
		data.horizontalAlignment= GridData.FILL;
		data.verticalAlignment= GridData.CENTER;
		fFilterText.setLayoutData(data);

		fFilterText.addKeyListener(new KeyListener() {
			public void keyPressed(KeyEvent e) {
				if (e.keyCode == 0x0D) // return
					gotoSelectedElement();
				if (e.keyCode == SWT.ARROW_DOWN)
					fTableViewer.getTable().setFocus();
				if (e.keyCode == SWT.ARROW_UP)
					fTableViewer.getTable().setFocus();
				if (e.character == 0x1B) // ESC
					dispose();
			}
			public void keyReleased(KeyEvent e) {
				// do nothing
			}
		});

		return fFilterText;
	}

	protected void createHorizontalSeparator(Composite parent) {
		Label separator= new Label(parent, SWT.SEPARATOR | SWT.HORIZONTAL | SWT.LINE_DOT);
		separator.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
	}

	protected void updateStatusFieldText() {
		setInfoText(getStatusFieldText());
	}

	protected String getStatusFieldText() {
		return ""; //$NON-NLS-1$
	}

	private void installFilter() {
		fFilterText.setText(""); //$NON-NLS-1$

		fFilterText.addModifyListener(new ModifyListener() {
			public void modifyText(ModifyEvent e) {
				fTableViewer.refresh();
			}
		});
	}


	protected StringMatcher getMatcher() {
		return fStringMatcher;
	}

	/**
	 * Implementers can modify
	 *
	 * @return the selected element
	 */
	protected Object getSelectedElement() {
		if (fTableViewer == null)
			return null;

		return ((IStructuredSelection) fTableViewer.getSelection()).getFirstElement();
	}

	private void gotoSelectedElement() {
		Object selectedElement= getSelectedElement();
		if (selectedElement instanceof IHyperlink) {
			((IHyperlink)selectedElement).open();
		}
	}

	/**
	 * Selects the first element in the table which
	 * matches the current filter pattern.
	 */
	protected void selectFirstMatch() {
		Object selectedElement= fTableViewer.testFindItem(fInitiallySelectedType);
		TableItem element;
		final Table table = fTableViewer.getTable();
		if (selectedElement instanceof TableItem)
			element= findElement(new TableItem[] { (TableItem)selectedElement });
		else
			element= findElement(table.getItems());

		if (element != null) {
			table.setSelection(element);
			table.showItem(element);
		} else
			fTableViewer.setSelection(StructuredSelection.EMPTY);
	}

	private TableItem findElement(TableItem[] items) {
		return findElement(items, null, true);
	}

	private TableItem findElement(TableItem[] items, TableItem[] toBeSkipped, boolean allowToGoUp) {
		if (fStringMatcher == null)
			return items.length > 0 ? items[0] : null;

		ILabelProvider labelProvider= (ILabelProvider)fTableViewer.getLabelProvider();

		// First search at same level
		for (int i= 0; i < items.length; i++) {
			final TableItem item= items[i];
			IHyperlink element= (IHyperlink)item.getData();
			if (element != null) {
				String label= labelProvider.getText(element);
				if (fStringMatcher.match(label))
					return item;
			}
		}

		return null;
	}
	
	/**
	 * {@inheritDoc}
	 */
	public void setInformation(String information) {
		// this method is ignored, see IInformationControlExtension2
	}

	/**
	 * {@inheritDoc}
	 */
	public abstract void setInput(Object information);

	/**
	 * Fills the view menu.
	 * Clients can extend or override.
	 *
	 * @param viewMenu the menu manager that manages the menu
	 * @since 3.0
	 */
	protected void fillViewMenu(IMenuManager viewMenu) {
		//fCustomFiltersActionGroup.fillViewMenu(viewMenu);
	}

	/*
	 * Overridden to call the old framework method.
	 *
	 * @see org.eclipse.jface.dialogs.PopupDialog#fillDialogMenu(IMenuManager)
	 * @since 3.2
	 */
	@Override
	protected void fillDialogMenu(IMenuManager dialogMenu) {
		super.fillDialogMenu(dialogMenu);
		fillViewMenu(dialogMenu);
	}

	protected void inputChanged(Object newInput, Object newSelection) {
		fFilterText.setText(""); //$NON-NLS-1$
		fInitiallySelectedType= null;
		fTableViewer.setInput(newInput);
		if (newSelection != null)
			fTableViewer.setSelection(new StructuredSelection(newSelection));
	}

	/**
	 * {@inheritDoc}
	 */
	public void setVisible(boolean visible) {
		if (visible) {
			open();
		} else {
			removeHandlerAndKeyBindingSupport();
			saveDialogBounds(getShell());
			getShell().setVisible(false);
		}
	}

	/*
	 * @see org.eclipse.jface.dialogs.PopupDialog#open()
	 * @since 3.3
	 */
	@Override
	public int open() {
		addHandlerAndKeyBindingSupport();
		return super.open();
	}
	
	protected Control getFocusControl() {
		return fFilterText;
	}

	/**
	 * {@inheritDoc}
	 */
	public final void dispose() {
		close();
	}

	/**
	 * {@inheritDoc}
	 * @param event can be null
	 * <p>
	 * Subclasses may extend.
	 * </p>
	 */
	public void widgetDisposed(DisposeEvent event) {
		removeHandlerAndKeyBindingSupport();
		fTableViewer= null;
		fFilterText= null;
	}

	/**
	 * Adds handler and key binding support.
	 *
	 * @since 3.2
	 */
	protected void addHandlerAndKeyBindingSupport() {
		// Register action with command support
		if (fShowViewMenuHandlerSubmission == null) {
			fShowViewMenuHandlerSubmission= new HandlerSubmission(null, getShell(), null, fShowViewMenuAction.getActionDefinitionId(), new ActionHandler(fShowViewMenuAction), Priority.MEDIUM);
			PlatformUI.getWorkbench().getCommandSupport().addHandlerSubmission(fShowViewMenuHandlerSubmission);
		}
	}

	/**
	 * Removes handler and key binding support.
	 *
	 * @since 3.2
	 */
	protected void removeHandlerAndKeyBindingSupport() {
		// Remove handler submission
		if (fShowViewMenuHandlerSubmission != null)
			PlatformUI.getWorkbench().getCommandSupport().removeHandlerSubmission(fShowViewMenuHandlerSubmission);

	}

	/**
	 * {@inheritDoc}
	 */
	public boolean hasContents() {
		return fTableViewer != null && fTableViewer.getInput() != null;
	}

	/**
	 * {@inheritDoc}
	 */
	public void setSizeConstraints(int maxWidth, int maxHeight) {
		// ignore
	}

	/**
	 * {@inheritDoc}
	 */
	public Point computeSizeHint() {
		// return the shell's size - note that it already has the persisted size if persisting
		// is enabled.
		return getShell().getSize();
	}

	/**
	 * {@inheritDoc}
	 */
	public void setLocation(Point location) {
		/*
		 * If the location is persisted, it gets managed by PopupDialog - fine. Otherwise, the location is
		 * computed in Window#getInitialLocation, which will center it in the parent shell / main
		 * monitor, which is wrong for two reasons:
		 * - we want to center over the editor / subject control, not the parent shell
		 * - the center is computed via the initalSize, which may be also wrong since the size may
		 *   have been updated since via min/max sizing of AbstractInformationControlManager.
		 * In that case, override the location with the one computed by the manager. Note that
		 * the call to constrainShellSize in PopupDialog.open will still ensure that the shell is
		 * entirely visible.
		 */
		if (!getPersistLocation() || getDialogSettings() == null)
			getShell().setLocation(location);
	}

	/**
	 * {@inheritDoc}
	 */
	public void setSize(int width, int height) {
		getShell().setSize(width, height);
	}

	/**
	 * {@inheritDoc}
	 */
	public void addDisposeListener(DisposeListener listener) {
		getShell().addDisposeListener(listener);
	}

	/**
	 * {@inheritDoc}
	 */
	public void removeDisposeListener(DisposeListener listener) {
		getShell().removeDisposeListener(listener);
	}

	/**
	 * {@inheritDoc}
	 */
	public void setForegroundColor(Color foreground) {
		applyForegroundColor(foreground, getContents());
	}

	/**
	 * {@inheritDoc}
	 */
	public void setBackgroundColor(Color background) {
		applyBackgroundColor(background, getContents());
	}

	/**
	 * {@inheritDoc}
	 */
	public boolean isFocusControl() {
		return getShell().getDisplay().getActiveShell() == getShell();
	}

	/**
	 * {@inheritDoc}
	 */
	public void setFocus() {
		getShell().forceFocus();
		fFilterText.setFocus();
	}

	/**
	 * {@inheritDoc}
	 */
	public void addFocusListener(FocusListener listener) {
		getShell().addFocusListener(listener);
	}

	/**
	 * {@inheritDoc}
	 */
	public void removeFocusListener(FocusListener listener) {
		getShell().removeFocusListener(listener);
	}

	final protected ICommand getInvokingCommand() {
		return fInvokingCommand;
	}

	final protected KeySequence[] getInvokingCommandKeySequences() {
		if (fInvokingCommandKeySequences == null) {
			if (getInvokingCommand() != null) {
				List<IKeySequenceBinding> list= getInvokingCommand().getKeySequenceBindings();
				if (!list.isEmpty()) {
					fInvokingCommandKeySequences= new KeySequence[list.size()];
					for (int i= 0; i < fInvokingCommandKeySequences.length; i++) {
						fInvokingCommandKeySequences[i]= list.get(i).getKeySequence();
					}
					return fInvokingCommandKeySequences;
				}
			}
		}
		return fInvokingCommandKeySequences;
	}

	/*
	 * @see org.eclipse.jface.dialogs.PopupDialog#getDialogSettings()
	 */
	@Override
	protected IDialogSettings getDialogSettings() {
		String sectionName= getId();

		IDialogSettings settings= JavaPlugin.getDefault().getDialogSettings().getSection(sectionName);
		if (settings == null)
			settings= JavaPlugin.getDefault().getDialogSettings().addNewSection(sectionName);

		return settings;
	}

	/*
	 * Overridden to insert the filter text into the title and menu area.
	 *
	 * @since 3.2
	 */
	@Override
	protected Control createTitleMenuArea(Composite parent) {
		fViewMenuButtonComposite= (Composite) super.createTitleMenuArea(parent);

		// If there is a header, then the filter text must be created
		// underneath the title and menu area.

		if (hasHeader()) {
			fFilterText= createFilterText(parent);
		}

		// Create show view menu action
		fShowViewMenuAction= new Action("showViewMenu") { //$NON-NLS-1$
			/*
			 * @see org.eclipse.jface.action.Action#run()
			 */
			@Override
			public void run() {
				showDialogMenu();
			}
		};
		fShowViewMenuAction.setEnabled(true);
		fShowViewMenuAction.setActionDefinitionId(IWorkbenchCommandConstants.WINDOW_SHOW_VIEW_MENU);

		return fViewMenuButtonComposite;
	}

	/*
	 * Overridden to insert the filter text into the title control
	 * if there is no header specified.
	 * @since 3.2
	 */
	@Override
	protected Control createTitleControl(Composite parent) {
		if (hasHeader()) {
			return super.createTitleControl(parent);
		}
		fFilterText= createFilterText(parent);
		return fFilterText;
	}

	/*
	 * @see org.eclipse.jface.dialogs.PopupDialog#setTabOrder(org.eclipse.swt.widgets.Composite)
	 */
	@Override
	protected void setTabOrder(Composite composite) {
		if (hasHeader()) {
			composite.setTabList(new Control[] { fFilterText, fTableViewer.getTable() });
		} else {
			fViewMenuButtonComposite.setTabList(new Control[] { fFilterText });
			composite.setTabList(new Control[] { fViewMenuButtonComposite, fTableViewer.getTable() });
		}
	}
}
