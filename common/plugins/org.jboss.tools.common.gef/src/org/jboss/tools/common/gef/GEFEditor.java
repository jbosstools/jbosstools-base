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
package org.jboss.tools.common.gef;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.IOException;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.EventObject;
import java.util.List;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IResourceChangeEvent;
import org.eclipse.core.resources.IResourceChangeListener;
import org.eclipse.core.resources.IResourceDelta;
import org.eclipse.core.resources.IResourceDeltaVisitor;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.QualifiedName;
import org.eclipse.draw2d.FigureCanvas;
import org.eclipse.draw2d.IFigure;
import org.eclipse.draw2d.text.FlowContext;
import org.eclipse.draw2d.text.SimpleTextLayout;
import org.eclipse.draw2d.text.TextFlow;
import org.eclipse.gef.ContextMenuProvider;
import org.eclipse.gef.DefaultEditDomain;
import org.eclipse.gef.GraphicalViewer;
import org.eclipse.gef.KeyHandler;
import org.eclipse.gef.dnd.TemplateTransferDragSourceListener;
import org.eclipse.gef.editparts.ScalableFreeformRootEditPart;
import org.eclipse.gef.editparts.ZoomManager;
import org.eclipse.gef.palette.PaletteContainer;
import org.eclipse.gef.palette.PaletteRoot;
import org.eclipse.gef.ui.palette.PaletteContextMenuProvider;
import org.eclipse.gef.ui.palette.PaletteViewer;
import org.eclipse.gef.ui.palette.PaletteViewerPreferences;
import org.eclipse.gef.ui.parts.GraphicalEditor;
import org.eclipse.gef.ui.parts.ScrollingGraphicalViewer;
import org.eclipse.gef.ui.parts.TreeViewer;
import org.eclipse.gef.ui.stackview.CommandStackInspectorPage;
import org.eclipse.jface.action.IMenuListener;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionProvider;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.KeyListener;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseListener;
import org.eclipse.swt.events.MouseTrackListener;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IFileEditorInput;
import org.eclipse.ui.IPartListener;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.IWorkbenchPartSite;
import org.eclipse.ui.part.FileEditorInput;
import org.eclipse.ui.views.contentoutline.IContentOutlinePage;
import org.jboss.tools.common.gef.action.IDiagramSelectionProvider;
import org.jboss.tools.common.gef.outline.xpl.DiagramContentOutlinePage;
import org.jboss.tools.common.gef.xpl.GEFSplitter;

public class GEFEditor extends GraphicalEditor implements MouseListener,
		MouseTrackListener, KeyListener {

	protected static final int PALETTE_MIN_SIZE = 23;

	private static final QualifiedName PALETTE_SIZE_KEY = new QualifiedName("",
			"palette_size");

	private static final QualifiedName ZOOM_SIZE_KEY = new QualifiedName("",
			"zoom_size");

	protected int lastPaletteLayout = -1;

	protected PaletteViewer paletteViewer;

	protected IEditorInput input;

	/**
	 * @deprecated
	 */
	protected static final int PALETTE_SIZE = 23;

	protected void setPaletteViewer(PaletteViewer paletteViewer) {
		this.paletteViewer = paletteViewer;
	}

	protected void initializeGraphicalViewer() {

	}

	public boolean isBordersPaint() {
		return true;
	}

	protected void createGraphicalViewer(Composite parent) {
		GraphicalViewer viewer = new GEFGraphicalViewer(this);
		viewer.createControl(parent);
		setGraphicalViewer(viewer);
		configureGraphicalViewer();
		hookGraphicalViewer();
		initializeGraphicalViewer();
	}

	protected PaletteViewer getPaletteViewer() {
		return paletteViewer;
	}

	public void createPartControl(Composite parent) {
		GEFSplitter splitter = new GEFSplitter(parent, SWT.HORIZONTAL,
				PALETTE_MIN_SIZE);
		createPaletteViewer(splitter);
		createGraphicalViewer(splitter);
		splitter.maintainSize(getPaletteViewer().getControl());
		splitter.setFixedSize(loadPaletteSize());
		splitter.addFixedSizeChangeListener(new PropertyChangeListener() {
			public void propertyChange(PropertyChangeEvent evt) {
				int size = ((GEFSplitter) evt.getSource()).getFixedSize();
				setPaletteLayout(paletteViewer.getPaletteViewerPreferences(),
						size);
				savePaletteSize(size);
			}
		});
		splitter.getChildren()[1].addMouseListener(this);
		splitter.getChildren()[1].addMouseTrackListener(this);
	}

	protected void setPaletteLayout(PaletteViewerPreferences prefs, int size) {
		int paletteLayout = size > PALETTE_MIN_SIZE ? PaletteViewerPreferences.LAYOUT_LIST
				: PaletteViewerPreferences.LAYOUT_ICONS;
		if (paletteLayout != lastPaletteLayout) {
			lastPaletteLayout = paletteLayout;
			prefs.setLayoutSetting(paletteLayout);
			FigureCanvas canvas = (FigureCanvas) paletteViewer.getControl();
			makeUnwrapPaletteItems(canvas.getContents());
		}
	}

	protected void makeUnwrapPaletteItems(IFigure figure) {
		if (figure instanceof TextFlow) {
			TextFlow textFlow = (TextFlow) figure;
			SimpleTextLayout layout = new SimpleTextLayout((TextFlow) figure);
			figure.setLayoutManager(layout);
			textFlow.setFlowContext((FlowContext) textFlow.getParent()
					.getLayoutManager());
		} else {
			List children = figure.getChildren();
			for (int i = 0; i < children.size(); i++) {
				makeUnwrapPaletteItems((IFigure) children.get(i));
			}
		}
	}

	protected void createPaletteViewer(Composite parent) {
	}

	private KeyHandler sharedKeyHandler;

	private PaletteRoot root;

	protected boolean savePreviouslyNeeded = false;

	private IPartListener partListener = new IPartListener() {
		public void partActivated(IWorkbenchPart part) {
			if (part != GEFEditor.this)
				return;
		}

		public void partBroughtToTop(IWorkbenchPart part) {
		}

		public void partClosed(IWorkbenchPart part) {
		}

		public void partDeactivated(IWorkbenchPart part) {
		}

		public void partOpened(IWorkbenchPart part) {
		}
	};

	public GEFEditor(IEditorInput input) {
		this.input = input;
		setEditDomain(new DefaultEditDomain(this));
	}

	/**
	 * @deprecated - for compatibility
	 */
	public GEFEditor() {
		setEditDomain(new DefaultEditDomain(this));
	}

	protected void closeEditor(boolean save) {
		getSite().getPage().closeEditor(GEFEditor.this, save);
	}

	public void commandStackChanged(EventObject event) {
		if (isDirty()) {
			if (!savePreviouslyNeeded()) {
				setSavePreviouslyNeeded(true);
				firePropertyChange(IEditorPart.PROP_DIRTY);
			}
		} else {
			setSavePreviouslyNeeded(false);
			firePropertyChange(IEditorPart.PROP_DIRTY);
		}
		super.commandStackChanged(event);
	}

	/**
	 * @see org.eclipse.gef.ui.parts.GraphicalEditorWithPalette#configurePaletteViewer()
	 */
	protected void configurePaletteViewer() {
		PaletteViewer viewer = (PaletteViewer) getPaletteViewer();
		ContextMenuProvider provider = new PaletteContextMenuProvider(viewer);
		getPaletteViewer().setContextMenu(provider);
	}

	ScrollingGraphicalViewer viewer;

	public ScrollingGraphicalViewer getScrollingGraphicalViewer() {
		return viewer;
	}

	protected void configureGraphicalViewer() {
	}

	public void mouseEnter(MouseEvent e) {
	}

	public void mouseExit(MouseEvent e) {
	}

	public void mouseHover(MouseEvent e) {
	}

	public void mouseDoubleClick(MouseEvent e) {
	}

	public void mouseDown(MouseEvent e) {
	}

	public void mouseUp(MouseEvent e) {
	}

	public void keyPressed(KeyEvent e) {
	}

	public void keyReleased(KeyEvent e) {
	}

	public Control getControl() {
		return this.getPaletteViewer().getControl();
	}

	protected void createOutputStream(OutputStream os) throws IOException {
	}

	public void dispose() {
		getSite().getWorkbenchWindow().getPartService().removePartListener(
				partListener);
		partListener = null;
		super.dispose();
	}

	public void doSave(IProgressMonitor progressMonitor) {
	}

	public void doSaveAs() {
	}

	public Object getAdapter(Class type) {
		if (type == IDiagramSelectionProvider.class) {
			if (getScrollingGraphicalViewer() == null)
				return null;
			return new IDiagramSelectionProvider() {
				public ISelection getSelection() {
					if (getScrollingGraphicalViewer() == null)
						return null;
					return getScrollingGraphicalViewer().getSelection();
				}
			};
		}
		if (type == CommandStackInspectorPage.class)
			return new CommandStackInspectorPage(getCommandStack());
		if (type == IContentOutlinePage.class) {
			DiagramContentOutlinePage outline = new DiagramContentOutlinePage(
					new TreeViewer());
			outline.setGraphicalViewer(getGraphicalViewer());
			outline.setSelectionSynchronizer(getSelectionSynchronizer());
			return outline;
		}

		if (type == ZoomManager.class) {
			if (getGraphicalViewer() != null)
				return ((ScalableFreeformRootEditPart) getGraphicalViewer()
						.getRootEditPart()).getZoomManager();
		}
		return super.getAdapter(type);
	}

	/**
	 * @see org.eclipse.gef.ui.parts.GraphicalEditorWithPalette#getInitialPaletteSize()
	 */
	protected int getInitialPaletteSize() {
		return 22;
	}

	/**
	 * @see org.eclipse.gef.ui.parts.GraphicalEditorWithPalette#handlePaletteResized(int)
	 */
	protected void handlePaletteResized(int newSize) {
	}

	/**
	 * Returns the KeyHandler with common bindings for both the Outline and
	 * Graphical Views. For example, delete is a common action.
	 */
	protected KeyHandler getCommonKeyHandler() {
		return sharedKeyHandler;
	}

	public ISelectionProvider getModelSelectionProvider() {
		return null;
	}

	protected PaletteRoot createPalette() {
		PaletteRoot palette = new PaletteRoot();
		palette.addAll(createCategories(palette));
		return palette;
	}

	protected PaletteContainer createControlGroup(PaletteRoot root) {
		return null;
	}

	protected List createCategories(PaletteRoot root) {
		List categories = new ArrayList();

		categories.add(createControlGroup(root));
		return categories;
	}

	protected PaletteRoot getPaletteRoot() {
		if (root == null) {
			root = createPalette();
		}
		return root;
	}

	public void gotoMarker(IMarker marker) {
	}

	protected void hookPaletteViewer() {
		getEditDomain().setPaletteViewer(paletteViewer);
		getPaletteViewer().getContextMenu().addMenuListener(
				new IMenuListener() {
					public void menuAboutToShow(IMenuManager manager) {
						manager.removeAll();
					}
				});
	}

	protected void initializePaletteViewer() {
		getEditDomain().setPaletteRoot(getPaletteRoot());
		getPaletteViewer().addDragSourceListener(
				new TemplateTransferDragSourceListener(getPaletteViewer()));
		getPaletteViewer().getPaletteViewerPreferences().setLayoutSetting(
				PaletteViewerPreferences.LAYOUT_LIST);
	}

	protected void createActions() {
	}

	public boolean isDirty() {
		return isSaveOnCloseNeeded();
	}

	public boolean isSaveAsAllowed() {
		return true;
	}

	public boolean isSaveOnCloseNeeded() {
		return getCommandStack().isDirty();
	}

	protected boolean performSaveAs() {
		return false;
	}

	private boolean savePreviouslyNeeded() {
		return savePreviouslyNeeded;
	}

	private void setSavePreviouslyNeeded(boolean value) {
		savePreviouslyNeeded = value;
	}

	protected void superSetInput(IEditorInput input) {
		super.setInput(input);
	}

	protected void setSite(IWorkbenchPartSite site) {
		super.setSite(site);
		getSite().getWorkbenchWindow().getPartService().addPartListener(
				partListener);
	}

	private IFile getFile() {
		return (input instanceof IFileEditorInput) ? ((IFileEditorInput) input)
				.getFile() : null;
	}

	protected int loadPaletteSize() {
		IFile file = getFile();
		int size = PALETTE_MIN_SIZE;
		if (file == null)
			return size;
		try {
			String s = file.getPersistentProperty(PALETTE_SIZE_KEY);
			if (s != null) {
				size = Integer.parseInt(s);
			}
		} catch (Exception e) {
		}
		return size;
	}

	protected void savePaletteSize(int fixedSise) {
		IFile file = getFile();
		if (file == null)
			return;
		try {
			String s = String.valueOf(fixedSise);
			file.setPersistentProperty(PALETTE_SIZE_KEY, s);
		} catch (Exception e) {
		}
	}

	protected double loadZoomSize() {
		IFile file = getFile();
		double size = 1.0;
		if (file == null)
			return size;
		try {
			String s = file.getPersistentProperty(ZOOM_SIZE_KEY);
			if (s != null) {
				size = Double.parseDouble(s);
			}
		} catch (Exception e) {
		}
		return size;
	}

	protected void saveZoomSize(double zoom) {
		IFile file = getFile();
		if (file == null)
			return;
		try {
			String s = String.valueOf(zoom);
			file.setPersistentProperty(ZOOM_SIZE_KEY, s);
		} catch (Exception e) {
		}
	}

	protected void hookGraphicalViewer() {
		getSelectionSynchronizer().addViewer(getGraphicalViewer());
	}

}