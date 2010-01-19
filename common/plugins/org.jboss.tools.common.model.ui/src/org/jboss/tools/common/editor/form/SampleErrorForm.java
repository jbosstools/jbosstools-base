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
package org.jboss.tools.common.editor.form;

import java.util.ArrayList;
import java.util.StringTokenizer;
import org.jboss.tools.common.editor.ErrorSelectionListener;
import org.eclipse.swt.SWT;
import org.eclipse.swt.SWTException;
import org.eclipse.swt.custom.StyleRange;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.events.MouseAdapter;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseMoveListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Cursor;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.ui.actions.ActionFactory;
import org.jboss.tools.common.model.ui.ModelUIPlugin;
import org.jboss.tools.common.model.ui.forms.ExpandableForm;
import org.jboss.tools.common.model.ui.widgets.IWidgetSettings;

/**
 * @author Aleksey
 */

public class SampleErrorForm extends ExpandableForm {
	static Color RED = Display.getDefault().getSystemColor(SWT.COLOR_RED);
	static Color BLUE = Display.getDefault().getSystemColor(SWT.COLOR_BLUE);
	static Color BLACK = Display.getDefault().getSystemColor(SWT.COLOR_BLACK);
	static Color WHITE = Display.getDefault().getSystemColor(SWT.COLOR_WHITE);
	
	private Composite control;
	private Composite superControl;
	private Composite contentComposite;
	private String errorsString;
	private boolean visible;

	private static final int ERROR_TYPE = 0;
	private static final int ERROR_LOCATION = 1;
	private static final int ERROR_MESSAGE = 2;
	ErrorSelectionListener listener;
	StyledText styledText = null;
	StyleRange[] regions = new StyleRange[0];
	

	public SampleErrorForm() {
		super();
		setHeadingText("Errors");
		setCollapsable(Boolean.FALSE.booleanValue());
	}

	public void addErrorSelectionListener(ErrorSelectionListener listener) {
		this.listener = listener;
	}

	/*
	 * @param is slava-packed error string
	 * 
	 * format:
	 * 
	 * ERROR_TYPE@ERROR_LOCATION@ERROR_MESSAGE_1\n
	 * ERROR_TYPE@ERROR_LOCATION@ERROR_MESSAGE_2\n
	 * ERROR_TYPE@ERROR_LOCATION@ERROR_MESSAGE_3
	 * or
	 * ERROR_MESSAGE_1\n
	 * ERROR_MESSAGE_2\n
	 * ERROR_MESSAGE_3
	 * 
	 * when specify only ERROR_MESSAGE, by default ERROR_TYPE = 'ERROR', ERROR_LOCATION = 0:0
	 * 
	 */
	
	public void initialize(Object model) {
		String newErrors = (String)model;
		if (newErrors==null) newErrors = ""; //$NON-NLS-1$
		if (newErrors.equals(errorsString)) return;
		this.errorsString = newErrors;
		disposeLabels();
		createLabels();		
	}

	public boolean isVisible() {
		return visible;
	}

	public void setVisible(boolean b) {
		if (visible == b) return;
		visible = b;
		if (getControl()!=null && !getControl().isDisposed()) {
			control.setVisible(visible);
			
			control.setRedraw(false);
			control.getParent().setRedraw(false);
			control.layout(true);
			control.getParent().layout(true);
			control.setRedraw(true);
			control.getParent().setRedraw(true);
			
		}
	}
	
	public Control createControl(Composite parent, IWidgetSettings factory) {
		if (control==null) {
			control = new Composite(parent, SWT.NONE);
			Font f = parent.getFont();
			if(f != null) {
				control.setFont(f);
			}
			
			ErrorLayout layout = new ErrorLayout();
			control.setLayout(layout);
			control.setLayoutData(getLayoutData());
			control.setBackground(parent.getBackground());
			
			// super
			superControl = (Composite)super.createControl(control, factory);
			//superControl.setLayout(getLayout());
			superControl.setLayoutData(new GridData(GridData.FILL_BOTH));
			if(f != null) {
				superControl.setFont(f);
			}
		}
		
		return control;
	}
	

	protected Control createClientArea(Composite parent, IWidgetSettings factory) {
		contentComposite = new Composite(parent, SWT.NONE);
		contentComposite.setBackground(parent.getBackground());
		GridLayout layout = new GridLayout();
		layout.marginHeight = 0;
		layout.marginWidth = 0;
		layout.horizontalSpacing = 0;
		layout.verticalSpacing = 0;
		contentComposite.setLayout(layout);
		contentComposite.setLayoutData(new GridData(GridData.FILL_BOTH));
		
		if (regions != null && regions.length > 0) {
			disposeLabels();
			createLabels();
		} 

		return contentComposite;
	}
	
	private StyledText getStyledText(int errorCount) {
		boolean needsScroll = errorCount > 4;
		int style = SWT.NONE | SWT.READ_ONLY;
		if(needsScroll) style |= SWT.V_SCROLL;
		if(styledText == null || styledText.isDisposed()) {
			return createStyledText(style);
		}
		if(needsScroll != ((styledText.getStyle() & SWT.V_SCROLL) != 0)) {
			styledText.dispose();
			return createStyledText(style);
		}
		return styledText;
	}
	
	private StyledText createStyledText(int style) {
		styledText = new StyledText(contentComposite, style);
		styledText.setCaret(null);
		GridData d = new GridData(GridData.FILL_BOTH);
		styledText.setLayoutData(d);
		ML ml = new ML();
		styledText.addMouseMoveListener(ml);
		styledText.addMouseListener(ml);
		return styledText;
	}
	
	private void createLabels() {
		if (this.contentComposite == null) return;
		String[] errors = getErrorMessages(this.errorsString);
		getStyledText(errors.length);
		ArrayList<StyleRange> regionList = new ArrayList<StyleRange>();
		String[] messages;
		StringBuffer sb = new StringBuffer();
		for (int i = 0; i < errors.length; ++i) {
			messages = parseErrorMessage(errors[i]);
			int a = sb.length();
			sb.append(messages[ERROR_TYPE]);
			StyleRange region = new StyleRange(a, sb.length() - a, RED, null);
			regionList.add(region);

			a = sb.length();
			sb.append(messages[ERROR_LOCATION]);
			region = new StyleRange2(a, sb.length() - a, BLUE, null, messages[ERROR_LOCATION]);
			regionList.add(region);

			a = sb.length();
			sb.append(messages[ERROR_MESSAGE]);
			region = new StyleRange(a, sb.length() - a, BLACK, null);
			regionList.add(region);

			if(i < errors.length - 1) {
				a = sb.length();
				sb.append("\n"); //$NON-NLS-1$
				region = new StyleRange(a, sb.length() - a, WHITE, null);
				regionList.add(region);
			}

		}
		styledText.setText(sb.toString());
		regions = (StyleRange[])regionList.toArray(new StyleRange[0]);
		styledText.setStyleRanges(regions);
		control.update();
		control.layout();
		control.getParent().update();
		control.getParent().layout();
	}
	
	class StyleRange2 extends StyleRange {
		int line = 0;
		int position = 0;
		public StyleRange2(int start, int length, Color fg, Color bg, String location) {
			super(start, length, fg, bg);
			int i = location.indexOf(":"); //$NON-NLS-1$
			line = i < 0 ? 0 : getInt(location.substring(0, i), 0);
			position = i < 0 ? 0 : getInt(location.substring(i + 1), 0);
		}
		public void execute() {
			if(listener != null) listener.errorSelected(line, position);
		}
	}
	
	int getInt(String s, int def) {
		if(s == null || s.length() == 0) return def;
		try {
			return Integer.parseInt(s);
		} catch (NumberFormatException e) {
			ModelUIPlugin.getPluginLog().logError(e);
		}
		return def;
	}
	
	class ML extends MouseAdapter implements MouseMoveListener {
		Cursor hand = new Cursor(null, SWT.CURSOR_HAND);
		Cursor arrow = new Cursor(null, SWT.CURSOR_ARROW);
		public void mouseMove(MouseEvent e) {
			StyleRange range = getRange(e);
			if(range != null && range.foreground == BLUE) {
				styledText.setCursor(hand);
			} else {
				styledText.setCursor(arrow);
			}
		}
		public void mouseUp(MouseEvent e) {
			StyleRange range = getRange(e);
			if(range instanceof StyleRange2) {
				((StyleRange2)range).execute();
			}
		}
	}
	
	StyleRange getRange(MouseEvent e) {
		int offset = 0;
		try {
			offset = styledText.getOffsetAtLocation(new Point(e.x, e.y));
		} catch (SWTException exc) {
			return null;
		} catch (IllegalArgumentException e2) {
			return null;
		}
		return getRange(offset);
	}
	
	StyleRange getRange(int offset) {
		for (int i = 0; i < regions.length; i++) {
			if(regions[i].start <= offset && offset < regions[i].start + regions[i].length) {
				return regions[i];
			}
		}
		return null;
		
	}
	
	private void disposeLabels() {
		if (regions != null && regions.length > 0) {
			regions = new StyleRange[0];
			if(styledText != null && !styledText.isDisposed()) {
				styledText.setStyleRanges(regions);
				styledText.setText(""); //$NON-NLS-1$
			}
		}
	}
	
	public void dispose() {
		disposeLabels();
		// dispose contentComposite
		if (contentComposite!=null && !contentComposite.isDisposed()) contentComposite.dispose();
		contentComposite = null;
		if (control!=null && !control.isDisposed()) control.dispose();
		control = null;
		listener = null;
		super.dispose();
	}

	// error reader from slava
	private String[] getErrorMessages(String s) {
		if(s == null) s = ""; //$NON-NLS-1$
		StringTokenizer st = new StringTokenizer(s, "\n"); //$NON-NLS-1$
		String[] rs = new String[st.countTokens()];
		for (int i = 0; i < rs.length; i++) rs[i] = st.nextToken();
		return rs;		
	}

	// error message parser from slava
	private String[] parseErrorMessage(String s) {
		String[] result = new String[3];
//		int line = 1; 
//		int position = 1;
		int i = s.indexOf('@');
		int j = s.indexOf('@', i + 1);
		int k = s.indexOf('@', j + 1);
		if(k > j) {
			String q = s.substring(j + 1, k);
			result[ERROR_TYPE] = s.substring(0, i);
			result[ERROR_LOCATION] =  q;
			result[ERROR_MESSAGE] = s.substring(k + 1);
		} else {
			result[ERROR_TYPE] = "ERROR";
			result[ERROR_LOCATION] = "0:0"; //$NON-NLS-1$
			result[ERROR_MESSAGE] = s;
		}
		if(result[ERROR_LOCATION] == null || "0:0".equals(result[ERROR_LOCATION])) { //$NON-NLS-1$
			result[ERROR_LOCATION] = ""; //$NON-NLS-1$
		}
		return result;
	}
	
	class ErrorLayout extends Layout {
		protected Point computeSize(Composite composite, int wHint, int hHint, boolean flushCache) {
			if (!isVisible()) return new Point(SWT.DEFAULT,SWT.DEFAULT);
			Point size = superControl.computeSize(wHint, hHint, flushCache);
			if (getLayout() instanceof GridLayout) {
				GridLayout layout = (GridLayout)getLayout();
				size.x = size.x + layout.marginWidth*2;
				size.y = size.y + layout.marginHeight*2;
			}
					if(size.y > 100) size.y = 100;
			return size; 
		}

		protected void layout(Composite composite, boolean flushCache) {
			int x = 0;
			int y = 0;
			int width = composite.getClientArea().width;
			int height = composite.getClientArea().height;
			
			if (!isVisible()) return;
			if (getLayout() instanceof GridLayout) {
				GridLayout layout = (GridLayout)getLayout(); 
				x = layout.marginHeight;
				y = layout.marginWidth;
				width = width - x*2;
				height = height - y*2;
			}
			superControl.setBounds(x, y, width, height);
			superControl.layout();
			contentComposite.layout();
		}
	}

	public boolean doGlobalAction(String actionId) {
		if(styledText == null || styledText.isDisposed() || !styledText.isFocusControl()) {
			return false;
		}
		if(styledText != null && !styledText.isDisposed()) {
			if (ActionFactory.COPY.getId().equals(actionId)) {
				styledText.copy();
			}
		}
		return true;
	}
}
