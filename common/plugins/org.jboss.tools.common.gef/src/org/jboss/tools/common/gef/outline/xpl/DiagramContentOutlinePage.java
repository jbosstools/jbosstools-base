/*******************************************************************************
 * Copyright (c) 2000, 2005 IBM Corporation and others.
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
package org.jboss.tools.common.gef.outline.xpl;

import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.draw2d.LightweightSystem;
import org.eclipse.draw2d.MarginBorder;
import org.eclipse.draw2d.Viewport;
import org.eclipse.draw2d.parts.ScrollableThumbnail;
import org.eclipse.draw2d.parts.Thumbnail;
import org.eclipse.gef.EditPartViewer;
import org.eclipse.gef.GraphicalViewer;
import org.eclipse.gef.LayerConstants;
import org.eclipse.gef.RootEditPart;
import org.eclipse.gef.editparts.ScalableFreeformRootEditPart;
import org.eclipse.gef.editparts.ZoomManager;
import org.eclipse.gef.ui.parts.ContentOutlinePage;
import org.eclipse.gef.ui.parts.SelectionSynchronizer;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Canvas;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.part.IPageSite;
import org.eclipse.ui.part.PageBook;


public class DiagramContentOutlinePage extends ContentOutlinePage implements IAdaptable {
	//Warning: these icons may be moved from xpl!
	private static String OUTLINE_ICON_PATH  = "outline.gif";
	private static String OVERVIEW_ICON_PATH = "overview.gif";
	
	private org.eclipse.ui.views.contentoutline.ContentOutlinePage poutline;
	private GraphicalViewer graphicalViewer;
	private SelectionSynchronizer selectionSynchronizer;
	
	private PageBook pageBook;
	private Control outline;
	private Canvas overview;
	private IAction showOutlineAction, showOverviewAction;
	static final int ID_OUTLINE  = 0;
	static final int ID_OVERVIEW = 1;
	private Thumbnail thumbnail;
	IPageSite pSite;
		
	public DiagramContentOutlinePage(EditPartViewer viewer){
		super(viewer);
	}
	
	public void setTreeOutline(org.eclipse.ui.views.contentoutline.ContentOutlinePage poutline) {
		this.poutline = poutline;
	}
	
	public void setGraphicalViewer(GraphicalViewer graphicalViewer) {
		this.graphicalViewer = graphicalViewer;
	}
	
	public GraphicalViewer getGraphicalViewer() {
		return graphicalViewer;
	}
	
	public void setSelectionSynchronizer(SelectionSynchronizer selectionSynchronizer) {
		this.selectionSynchronizer = selectionSynchronizer;
	}
	
	public SelectionSynchronizer getSelectionSynchronizer() {
		return selectionSynchronizer;
	}
	
	public void init(IPageSite pageSite) {
		super.init(pageSite);
		this.pSite = pageSite;
	}

	protected void configureOutlineViewer(){
		IToolBarManager tbm = getSite().getActionBars().getToolBarManager();
		IMenuManager mm = getSite().getActionBars().getMenuManager();
		showOutlineAction = new Action() {
			public void run() {
				showPage(ID_OUTLINE);
			}
		};
		showOutlineAction.setImageDescriptor(ImageDescriptor.createFromFile(getClass(), OUTLINE_ICON_PATH));
		showOutlineAction.setToolTipText(GEFUIMessages.TREE);
		tbm.add(showOutlineAction);
		showOutlineAction.setText(GEFUIMessages.SHOW_TREE);
		mm.add(showOutlineAction);


		showOverviewAction = new Action() {
			public void run() {
				showPage(ID_OVERVIEW);
			}
		};
		showOverviewAction.setToolTipText(GEFUIMessages.DIAGRAM_NAVIGATOR);
		showOverviewAction.setImageDescriptor(ImageDescriptor.createFromFile(getClass(), OVERVIEW_ICON_PATH)); 
		tbm.add(showOverviewAction);
		
		showOverviewAction.setText(GEFUIMessages.SHOW_DIAGRAM_NAVIGATOR);
		mm.add(showOverviewAction);

		showPage(ID_OUTLINE);
	}

	public void createControl(Composite parent){
		pageBook = new PageBook(parent, SWT.NONE);
			
		poutline.init(pSite);
		poutline.createControl(pageBook);
		outline = poutline.getControl();	
									
		overview = new Canvas(pageBook, SWT.NONE);
		pageBook.showPage(outline);
		configureOutlineViewer();
		hookOutlineViewer();
		initializeOutlineViewer();
	}
	
	public void dispose(){
		if(poutline != null) {
			poutline.dispose();
			poutline = null;
		}
		unhookOutlineViewer();
		if (thumbnail != null)
			thumbnail.deactivate();
		super.dispose();
	}
	
	public Object getAdapter(Class type) {
		if (type == ZoomManager.class){
			return ((ScalableFreeformRootEditPart)getGraphicalViewer().getRootEditPart()).getZoomManager();
		}
		return null;
	}

	public Control getControl() {
		return pageBook;
	}

	protected void hookOutlineViewer(){
		getSelectionSynchronizer().addViewer(getViewer());
	}

	protected void initializeOutlineViewer(){
		//getViewer().setContents(getLogicDiagram());
	}
	
	protected void initializeOverview() {
		LightweightSystem lws = new LightweightSystem(overview);
		RootEditPart rep = getGraphicalViewer().getRootEditPart();
			
		if (rep instanceof ScalableFreeformRootEditPart) {
			ScalableFreeformRootEditPart root = (ScalableFreeformRootEditPart)rep;
			thumbnail = new ScrollableThumbnail((Viewport)root.getFigure());
			thumbnail.setBorder(new MarginBorder(3));
			thumbnail.setSource(root.getLayer(LayerConstants.PRINTABLE_LAYERS));
			lws.setContents(thumbnail);
		}
	}
	
	protected void showPage(int id) {
		if (id == ID_OUTLINE) {
			showOutlineAction.setChecked(true);
			showOverviewAction.setChecked(false);
			pageBook.showPage(outline);
			if (thumbnail != null)
				thumbnail.setVisible(false);
		} else if (id == ID_OVERVIEW) {
			if (thumbnail == null)
				initializeOverview();
			showOutlineAction.setChecked(false);
			showOverviewAction.setChecked(true);
			pageBook.showPage(overview);
			thumbnail.setVisible(true);
		}
	}
	
	protected void unhookOutlineViewer() {
		getSelectionSynchronizer().removeViewer(getViewer());
	}

}
