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
package org.jboss.tools.common.model.ui.reporting;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.File;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.text.Collator;
import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.Comparator;
import java.util.Date;
import java.util.Locale;
import java.util.StringTokenizer;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.Platform;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.DialogPage;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.resource.JFaceResources;
import org.eclipse.osgi.util.NLS;
import org.eclipse.swt.SWT;
import org.eclipse.swt.dnd.Clipboard;
import org.eclipse.swt.dnd.TextTransfer;
import org.eclipse.swt.dnd.Transfer;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.FontMetrics;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.about.ISystemSummarySection;
import org.eclipse.ui.internal.IWorkbenchConstants;
import org.eclipse.ui.internal.WorkbenchMessages;
import org.eclipse.ui.internal.WorkbenchPlugin;
import org.eclipse.ui.internal.registry.IWorkbenchRegistryConstants;
import org.jboss.tools.common.CommonPlugin;
import org.jboss.tools.common.meta.action.XActionInvoker;
import org.jboss.tools.common.meta.action.XEntityData;
import org.jboss.tools.common.meta.action.impl.XEntityDataImpl;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.options.PreferenceModelUtilities;
import org.jboss.tools.common.model.ui.ModelUIImages;
import org.jboss.tools.common.model.ui.ModelUIPlugin;
import org.jboss.tools.common.model.ui.attribute.XAttributeSupport;
import org.jboss.tools.common.model.ui.attribute.adapter.IModelPropertyEditorAdapter;
import org.jboss.tools.common.model.ui.messages.UIMessages;
import org.jboss.tools.common.model.ui.widgets.ReferenceListener;
import org.jboss.tools.common.model.ui.widgets.TextAndReferenceComponent;
import org.jboss.tools.common.model.ui.wizards.query.AbstractQueryWizard;
import org.jboss.tools.common.model.ui.wizards.query.AbstractQueryWizardView;
import org.jboss.tools.common.reporting.ProblemReportingHelper;

public class ReportProblemWizard extends AbstractQueryWizard {

	public ReportProblemWizard() {
		setView(new ReportProblemWizardView());
		getView().setMessage(UIMessages.REPORT_PROBLEM_DESCRIPTION);
	}

}

/**
 * @author yukhovich
 *
 */
class ReportProblemWizardView extends AbstractQueryWizardView {	
	/** LOG_DATE_FORMAT */
	final private static SimpleDateFormat LOG_DATE_FORMAT = new SimpleDateFormat(
			"yyyyMMddHHmmss"); //$NON-NLS-1$
	
	final private static SimpleDateFormat CURENT_DATE_FORMAT = new SimpleDateFormat(
	"yyyy-MM-dd"); //$NON-NLS-1$
	
	final private static SimpleDateFormat SESSION_DATE_FORMAT = new SimpleDateFormat(
	"yyyy-MM-dd HH:mm"); //$NON-NLS-1$
	
	final private static long TWENTY_FOUR_HOURS = 86400000;

	/** log file name */
	private Text logFileName;

	/** Browse button */
	private Button browseButton;

	/** LINE_LIMIT */
	final private static int LINE_LIMIT = 70;


	int stackTracesCount;
	Text problemDescription;
	XAttributeSupport sendSupport;
	XAttributeSupport infoSupport;
	
	XModelObject reportOptions = PreferenceModelUtilities.getPreferenceModel()
			.getByPath(ReportPreference.OPTIONS_REPORT_PROBLEM_PATH);
	
	XModelObject exceptionsObject = PreferenceModelUtilities
			.getPreferenceModel()
			.createModelObject("ProblemBufferEditor", null); //$NON-NLS-1$

	XEntityData sendData = XEntityDataImpl.create(new String[][] {
			{ "SharableReportProblem" }, //$NON-NLS-1$
			{ ReportPreference.ATT_ATTACH_REDHAT_LOG, "no" }, //$NON-NLS-1$
	// {ReportPreference.ATT_ATTACH_ECLIPSE_LOG, "no"}
			});
	XEntityData sendData2 = XEntityDataImpl
			.create(new String[][] { { "SharableReportProblem" }, //$NON-NLS-1$
			// {ReportPreference.ATT_ATTACH_ECLIPSE_LOG, "no"}
			});
	XEntityData infoData = XEntityDataImpl.create(new String[][] {
			{ "SharableReportProblem" }, { ReportPreference.ATT_E_MAIL, "no" }, //$NON-NLS-1$ //$NON-NLS-2$
			{ ReportPreference.ATT_OTHER, "no" } }); //$NON-NLS-1$

	class PixelConverter {

		private FontMetrics fFontMetrics;

		public PixelConverter(Control control) {
			GC gc = new GC(control);
			gc.setFont(control.getFont());
			fFontMetrics = gc.getFontMetrics();
			gc.dispose();
		}

		/**
		 * @see DialogPage#convertHorizontalDLUsToPixels
		 */
		public int convertHorizontalDLUsToPixels(int dlus) {
			return Dialog.convertHorizontalDLUsToPixels(fFontMetrics, dlus);
		}

		/**
		 * @see DialogPage#convertWidthInCharsToPixels
		 */
		public int convertWidthInCharsToPixels(int chars) {
			return Dialog.convertWidthInCharsToPixels(fFontMetrics, chars);
		}
	}

	public ReportProblemWizardView() {
		//folderLogFileName = Platform.getLocation().toOSString() + SEPARATOR;
	}

	public Control createControl(Composite parent) {
		Composite composite = new Composite(parent, SWT.NONE);
		Layout layout = new GridLayout();
		composite.setLayout(layout);
		createFilenameLogControl(composite);
		createStackTracesControl(composite);
		createProblemControl(composite);
		createContactInfoControl(composite);
		
		initializeData();
		 
		return composite;
	}


	/* (non-Javadoc)
	 * @see org.jboss.tools.common.model.ui.wizards.query.AbstractQueryWizardView#updateBar()
	 */
	public void updateBar() {
		getCommandBar().setEnabled(OK, !isMessageEmpty());
	}

	void validate() {
		if(problemDescription == null || problemDescription.isDisposed()) return;
		String text = problemDescription.getText();
		if(text == null || text.trim().length() == 0) {
			setErrorMessage(UIMessages.REPORT_PROBLEM_NO_DESCRIPTION);
		} else {
			setErrorMessage(null);
			setMessage(UIMessages.REPORT_PROBLEM_DESCRIPTION);
		}
	}

	/* (non-Javadoc)
	 * @see org.jboss.tools.common.model.ui.wizards.query.AbstractQueryWizardView#getPreferredSize()
	 */
	public Point getPreferredSize() {
		return new Point(750, 500);		
	}

	/* (non-Javadoc)
	 * @see org.jboss.tools.common.model.ui.wizards.query.AbstractQueryWizardView#getCommands()
	 */
	public String[] getCommands() {
		return new String[] { OK, CANCEL };
	}

	/* (non-Javadoc)
	 * @see org.jboss.tools.common.model.ui.wizards.query.AbstractQueryWizardView#action(java.lang.String)
	 */
	public void action(String command) {
		if (command.equals(OK)) {
			submitProblems();
			super.action(OK);
		} else {
			super.action(command);
		}
	}


	/**
	 * 
	 */
	private void initializeData() {
		File defaultLocation = new File(Platform.getLocation().toFile(), getDefaultZipFilename());
		logFileName.setText(defaultLocation.toString());
	}

	/**
	 * @return
	 */
	private byte[] getEclipseLogContent() {
		StringBuilder sb = new StringBuilder();
		String location = Platform.getLogFileLocation().toOSString();
		File f = new File(location);
		if(!f.isFile()) return sb.toString().getBytes();

		InputStreamReader in = null;
		try {
			in = new FileReader(location);

			char[] tempBuffer = new char[512];
			int len = 0;
			while ((len = in.read(tempBuffer)) != -1) {
				sb.append(tempBuffer);
			}

		} catch (IOException e) {
			ModelUIPlugin.getPluginLog().logError(e);
		}
		finally {
			if (in != null)	{
				try {
					in.close();
				} catch (Exception ignore) {
					//Ignore
				}
			}
		}

		return getLogByCurentDate(sb.toString()).getBytes();
	};
	
	private String getLogByCurentDate(String log){
		String lastOldSession=""; //$NON-NLS-1$
		boolean isAccept = false;
		Date today = new Date();

		StringBuffer sb = new StringBuffer();
		StringTokenizer st = new StringTokenizer(log, "\n", true); //$NON-NLS-1$
		while (st.hasMoreTokens()) {
			String t = st.nextToken();
			if(t.startsWith("!SESSION")){ //$NON-NLS-1$
				String dateString = t.substring(9, 25);
				try{
					Date date = SESSION_DATE_FORMAT.parse(dateString);
					long delta = today.getTime()-date.getTime();
					if(delta < TWENTY_FOUR_HOURS){
						if(!isAccept) sb.append(lastOldSession+"\n"); //$NON-NLS-1$
						sb.append(t);
						isAccept = true;
					}else{
						lastOldSession = new String(t);
						isAccept = false;
					}
				}catch(ParseException ex){
					//ModelUIPlugin.getPluginLog().logError(ex);
					sb.append(t);
					isAccept = true;
				}
			}else{
				if(isAccept){
					sb.append(t);
				}
			}
		}
		return sb.toString();
	}

	/**
	 * @return
	 */
	private byte[] getUserCommentContent() {
		StringBuffer sb = new StringBuffer();

		sb = sb.append("email : ").append( //$NON-NLS-1$
				ReportPreference.E_MAIL_OPTION.getValue()).append("\n"); //$NON-NLS-1$
		sb = sb.append("description : ").append(problemDescription.getText()) //$NON-NLS-1$
				.append("\n"); //$NON-NLS-1$
		sb.append("other : ").append(ReportPreference.OTHER_OPTION.getValue()) //$NON-NLS-1$
				.append("\n"); //$NON-NLS-1$

		return sb.toString().getBytes();
	}

	/**
	 * @return
	 */
	private byte[] getEclipsePropertiesContent() {
		StringWriter out = new StringWriter();
		PrintWriter writer = new PrintWriter(out);
		writer.println(NLS.bind(WorkbenchMessages.SystemSummary_timeStamp,
				DateFormat
						.getDateTimeInstance(DateFormat.FULL, DateFormat.FULL)
						.format(new Date())));

		appendExtensions(writer);
		writer.close();
		return out.toString().getBytes();
	}

	/**
	 * create a ZIP file contain following files: 
	 */
	private void createZipFile() throws IOException {
		
		File zipFilename = new File(logFileName.getText());
		
		ZipOutputStream zout = new ZipOutputStream(new FileOutputStream(zipFilename));
		try {
			addFileToZip(getEclipseLogContent(), "eclipse.log", zout); //$NON-NLS-1$
			addFileToZip(getUserCommentContent(), "usercomment.txt", zout); //$NON-NLS-1$
			addFileToZip(getEclipsePropertiesContent(), "eclipse.properties", zout); //$NON-NLS-1$

			ModelUIPlugin.getDefault().logInfo("Wrote diagnostic info to " + zipFilename); //$NON-NLS-1$
		} finally {
			zout.close();
		}
	}

	/**
	 * Add bytes form tempBuffer in zipOutputStream with name from fileName
	 * @param tempBuffer tempolary-buffer
	 * @param fileName
	 * @param zout
	 * @throws IOException
	 */
	private void addFileToZip(byte[] tempBuffer, String fileName,
			ZipOutputStream zout) throws IOException {
		
		ZipEntry e = new ZipEntry(fileName);
		zout.putNextEntry(e);
		zout.write(tempBuffer, 0, tempBuffer.length);
		zout.closeEntry();
	}


	/**
	 * Get a filename for zip-file
	 * 
	 * @return filename for zip-file
	 */
	private String getDefaultZipFilename() {
		String currentDate;
		Date today = new Date();

		currentDate = LOG_DATE_FORMAT.format(today);
		return "jbosstools-diagnostics-" + currentDate.toString() + ".zip"; //$NON-NLS-1$ //$NON-NLS-2$
	}
	
	private void createStackTracesControl(Composite parent) {
		stackTracesCount = ProblemReportingHelper.buffer.getSize();
		exceptionsObject.setAttributeValue("exceptions", //$NON-NLS-1$
				formatContent(ProblemReportingHelper.buffer.getContent()));
		XEntityData data = (stackTracesCount > 0) ? sendData : sendData2;
		boolean isSeparatorNeeded = false;
		if (stackTracesCount > 0) {
			createStackTracesLabelControl(parent);
			isSeparatorNeeded = true;
		}
		if (data.getAttributeData().length > 0) {
			sendSupport = new XAttributeSupport(reportOptions, data, true);
			sendSupport.setAutoStore(false);
			GridLayout layout = getDefaultSupportLayout();
			layout.marginWidth = 10;
			sendSupport.setLayout(layout);
			sendSupport.createControl(parent);
			sendSupport.addPropertyChangeListener(new PropertyChangeListener() {
				public void propertyChange(PropertyChangeEvent evt) {
					updateBar();
				}
			});
			isSeparatorNeeded = true;
		} else {
			sendSupport = null;
		}
		if (isSeparatorNeeded) {
			Label separator = new Label(parent, SWT.SEPARATOR | SWT.HORIZONTAL);
			separator.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
		}
	}


	/**
	 * 
	 * @param parent
	 *            a parent Composite object
	 */
	private void createFilenameLogControl(final Composite parent) {
		Group g = new Group(parent, SWT.SHADOW_ETCHED_IN);
		g.setLayoutData(new GridData(GridData.HORIZONTAL_ALIGN_FILL));

		GridLayout layout = new GridLayout(3, false);
		g.setLayout(layout);

		createLabel(g, "Log file name", 1);
		logFileName = createSingleText(g, 1);
		browseButton = createPushButton(g, "&Browse...", null);

		browseButton.addSelectionListener(new SelectionListener() {
			public void widgetDefaultSelected(SelectionEvent e) {
				widgetSelected(e);
			}

			public void widgetSelected(SelectionEvent e) {
				FileDialog fileDialog = new FileDialog(parent.getShell(),
						SWT.SAVE);
				fileDialog.setText("Save");


				File fileName = new File(logFileName.getText());
				
				fileDialog.setFilterPath(fileName.getParent());
				fileDialog.setFileName(fileName.getName());
				
				String[] filterExt = { "*.zip" }; //$NON-NLS-1$
				fileDialog.setFilterExtensions(filterExt);
				String selected = fileDialog.open();

				if (selected != null) {
					logFileName.setText(selected);					
				}
			};

		});
	}

	private void createProblemControl(Composite parent) {
		Group g = new Group(parent, SWT.SHADOW_ETCHED_IN);
		g.setText("Problem Description");
		g.setLayoutData(new GridData(GridData.FILL_BOTH));
		g.setLayout(new GridLayout());
		problemDescription = new Text(g, SWT.MULTI | SWT.H_SCROLL
				| SWT.V_SCROLL | SWT.BORDER);
		problemDescription.setLayoutData(new GridData(GridData.FILL_BOTH));
		problemDescription.addModifyListener(new ModifyListener() {
			public void modifyText(ModifyEvent e) {
				validate();
				updateBar();
			}
		});
	}

	private void createContactInfoControl(Composite parent) {
		Group g = new Group(parent, SWT.SHADOW_ETCHED_IN);
		g.setText("Contact Information (optional)");
		g.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
		g.setLayout(new GridLayout());
		infoSupport = new XAttributeSupport(reportOptions, infoData, true);
		infoSupport.setAutoStore(false);
		infoSupport.setLayout(getDefaultSupportLayout());
		Control c = infoSupport.createControl(g);
		c.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
		infoSupport.addPropertyChangeListener(new PropertyChangeListener() {			
			public void propertyChange(PropertyChangeEvent evt) {
				validate();				
			}
		});
	}

	private void createStackTracesLabelControl(Composite parent) {
		Composite panel = new Composite(parent, SWT.NONE);
		GridLayout layout = getDefaultSupportLayout();
		layout.marginWidth = 10;
		panel.setLayout(layout);
		Label image = new Label(panel, SWT.NONE);
		image.setImage(ModelUIImages.getImage("warning.gif")); //$NON-NLS-1$
		GridData d = new GridData();
		d.widthHint = 16;
		image.setLayoutData(d);
		image.setLayoutData(new GridData());
		TextAndReferenceComponent t2 = new TextAndReferenceComponent(panel,
				SWT.NONE);
		d = new GridData(GridData.FILL_HORIZONTAL);
		d.horizontalIndent = 5;
		t2.setLayoutData(d);
		String text = "You have " + stackTracesCount
				+ " stack traces to send (<a>see details</a>).";
		t2.setText(text, 300);
		t2.addReferenceListener(new ReferenceListener() {
			public void referenceSelected(String reference) {
				XActionInvoker.invoke("EditActions.Edit", exceptionsObject, //$NON-NLS-1$
						null);
			}
		});
	}

	private void submitProblems() {
		if (sendSupport != null)
			sendSupport.store();
		if (infoSupport != null)
			infoSupport.store();
		String text = CommonPlugin.getEnvironment() + "\n" //$NON-NLS-1$
				+ problemDescription.getText();
		boolean addRedHatLog = "yes" //$NON-NLS-1$
				.equals(ReportPreference.ATTACH_REDHAT_LOG_OPTION.getValue());
		if (addRedHatLog && stackTracesCount > 0) {
			String log = exceptionsObject.getAttributeValue("exceptions"); //$NON-NLS-1$
			text += "\n----------Red Hat Log-----------\n" + log + "\n"; //$NON-NLS-1$ //$NON-NLS-2$
		}
		boolean addEclipseLog = "yes" //$NON-NLS-1$
				.equals(ReportPreference.ATTACH_ECLIPSE_LOG_OPTION.getValue());
		if (addEclipseLog) {
			String eclipseLog = ProblemReportingHelper.buffer
					.getEclipseLogContent();
			text += "\n----------Eclipse Log----------\n" + eclipseLog + "\n"; //$NON-NLS-1$ //$NON-NLS-2$
		}
		/*
		String email = ReportPreference.E_MAIL_OPTION.getValue();
		String other = ReportPreference.OTHER_OPTION.getValue();
		*/
		try {
			createZipFile();
			// Submit.getInstance().submit(reportText, addRedHatLog);
			// ProblemReportingHelper.buffer.report(text, email, other,
			// addRedHatLog);
			showConfirmDialog();
		} catch (IOException e) {
			ModelUIPlugin.getPluginLog().logError(e);
		}

		if (addRedHatLog) {
			// clean is to be done after report,
			// which is executed as background job.
			// ProblemReportingHelper.buffer.clean();
		}
	}

	private void showConfirmDialog() {
		final String filename = logFileName.getText();
		String message = NLS.bind(UIMessages.REPORT_PROBLEM_RESULT, filename);
		
		MessageDialog dialog = new MessageDialog(problemDescription.getShell(),
				"Info",
				null,
				message,
				MessageDialog.INFORMATION,
				new String[]{UIMessages.REPORT_PROBLEM_COPY_BUTTON, "Close"},
				1) {
			protected void buttonPressed(int buttonId) {
				if(buttonId == 0) {
					Clipboard clipboard= new Clipboard(problemDescription.getShell().getDisplay());
					try {
						clipboard.setContents(new String[] { filename }, new Transfer[] { TextTransfer.getInstance() });
					} finally {
						clipboard.dispose();
					}
				}
				super.buttonPressed(buttonId);
			}
		};
		dialog.open();
	}

	private String formatContent(String content) {
		StringBuffer sb = new StringBuffer();
		StringTokenizer st = new StringTokenizer(content, "\n", true); //$NON-NLS-1$
		while (st.hasMoreTokens()) {
			String t = st.nextToken();
			if ("\n".equals(t)) { //$NON-NLS-1$
				sb.append(t);
			} else {
				while (t.length() > LINE_LIMIT) {
					String t1 = t.substring(0, LINE_LIMIT);
					int i = t1.lastIndexOf(' ');
					if (i > 40)
						t1 = t1.substring(0, i);
					t = t.substring(t1.length());
					sb.append(t1).append("\n"); //$NON-NLS-1$
				}
				sb.append(t);
			}
		}
		return sb.toString();
	}


	/**
	 * 
	 * @return
	 */
	private boolean isMessageEmpty() {
		if (sendSupport != null) {
			IModelPropertyEditorAdapter a = sendSupport
					.getPropertyEditorAdapterByName(ReportPreference.ATT_ATTACH_REDHAT_LOG);
			if (a != null && "yes".equals(a.getValue())) //$NON-NLS-1$
				return false;
			a = sendSupport
					.getPropertyEditorAdapterByName(ReportPreference.ATT_ATTACH_ECLIPSE_LOG);
			if (a != null && "yes".equals(a.getValue())) //$NON-NLS-1$
				return false;
		}
		String text = problemDescription.getText();
		if (text == null || text.trim().length() == 0)
			return true;
		return false;
	}

	
	/**
	 * Creates a new label widget
	 * 
	 * @param parent
	 *            the parent composite to add this label widget to
	 * @param text
	 *            the text for the label
	 * @param hspan
	 *            the horizontal span to take up in the parent composite
	 * @return the new label
	 */
	private Label createLabel(Composite parent, String text, int hspan) {
		Label l = new Label(parent, SWT.NONE);
		l.setFont(parent.getFont());
		l.setText(text);
		GridData gd = new GridData();
		gd.horizontalSpan = hspan;
		l.setLayoutData(gd);
		return l;
	}

	/**
	 * Creates a new text widget
	 * 
	 * @param parent
	 *            the parent composite to add this text widget to
	 * @param hspan
	 *            the horizontal span to take up on the parent composite
	 * @return the new text widget
	 */
	private Text createSingleText(Composite parent, int hspan) {
		Text t = new Text(parent, SWT.SINGLE | SWT.BORDER);
		t.setFont(parent.getFont());
		GridData gd = new GridData(GridData.FILL_HORIZONTAL);
		gd.horizontalSpan = hspan;
		gd.widthHint = 100;
		t.setLayoutData(gd);
		return t;
	}

	/**
	 * Returns a width hint for a button control.
	 */
	private int getButtonWidthHint(Button button) {
		button.setFont(JFaceResources.getDialogFont());
		PixelConverter converter = new PixelConverter(button);
		int widthHint = converter
				.convertHorizontalDLUsToPixels(IDialogConstants.BUTTON_WIDTH);
		return Math.max(widthHint, button.computeSize(SWT.DEFAULT, SWT.DEFAULT,
				true).x);
	}

	/**
	 * Sets width and height hint for the button control. <b>Note:</b> This is
	 * a NOP if the button's layout data is not an instance of
	 * <code>GridData</code>.
	 * 
	 * @param the
	 *            button for which to set the dimension hint
	 */
	private void setButtonDimensionHint(Button button) {
		Object gd = button.getLayoutData();
		if (gd instanceof GridData) {
			((GridData) gd).widthHint = getButtonWidthHint(button);
			((GridData) gd).horizontalAlignment = GridData.FILL;
		}
	}

	/**
	 * Creates and returns a new push button with the given label and/or image.
	 * 
	 * @param parent
	 *            parent control
	 * @param label
	 *            button label or <code>null</code>
	 * @param image
	 *            image or <code>null</code>
	 * 
	 * @return a new push button
	 */
	private Button createPushButton(Composite parent, String label, Image image) {
		Button button = new Button(parent, SWT.PUSH);
		button.setFont(parent.getFont());
		if (image != null) {
			button.setImage(image);
		}
		if (label != null) {
			button.setText(label);
		}
		GridData gd = new GridData();
		button.setLayoutData(gd);
		setButtonDimensionHint(button);
		return button;
	}

	private IConfigurationElement[] getSortedExtensions() {
		IConfigurationElement[] configElements = Platform
				.getExtensionRegistry().getConfigurationElementsFor(
						PlatformUI.PLUGIN_ID,
						IWorkbenchRegistryConstants.PL_SYSTEM_SUMMARY_SECTIONS);

		Arrays.sort(configElements, new Comparator() {
			Collator collator = Collator.getInstance(Locale.getDefault());

			public int compare(Object a, Object b) {
				IConfigurationElement element1 = (IConfigurationElement) a;
				IConfigurationElement element2 = (IConfigurationElement) b;

				String id1 = element1.getAttribute("id"); //$NON-NLS-1$
				String id2 = element2.getAttribute("id"); //$NON-NLS-1$

				if (id1 != null && id2 != null && !id1.equals(id2)) {
					return collator.compare(id1, id2);
				}

				String title1 = element1.getAttribute("sectionTitle"); //$NON-NLS-1$ 
				String title2 = element2.getAttribute("sectionTitle"); //$NON-NLS-1$

				if (title1 == null) {
					title1 = ""; //$NON-NLS-1$
				}
				if (title2 == null) {
					title2 = ""; //$NON-NLS-1$
				}

				return collator.compare(title1, title2);
			}
		});

		return configElements;
	}

	/*
	 * Appends the contents of all extensions to the configurationLogSections
	 * extension point.
	 */
	private void appendExtensions(PrintWriter writer) {
		IConfigurationElement[] configElements = getSortedExtensions();
		for (int i = 0; i < configElements.length; ++i) {
			IConfigurationElement element = configElements[i];

			Object obj = null;
			try {
				obj = WorkbenchPlugin.createExtension(element,
						IWorkbenchConstants.TAG_CLASS);
			} catch (CoreException e) {
				WorkbenchPlugin.log(
						"could not create class attribute for extension", //$NON-NLS-1$
						e.getStatus());
			}

			writer.println();
			writer.println(NLS.bind(
					WorkbenchMessages.SystemSummary_sectionTitle, element
							.getAttribute("sectionTitle"))); //$NON-NLS-1$

			if (obj instanceof ISystemSummarySection) {
				ISystemSummarySection logSection = (ISystemSummarySection) obj;
				logSection.write(writer);
			} else {
				writer.println(WorkbenchMessages.SystemSummary_sectionError);
			}
		}
	}
	
	protected GridLayout getDefaultSupportLayout() {
		GridLayout gridLayout = new GridLayout(2, false);
		gridLayout.marginHeight = 0;
		gridLayout.marginWidth = 0;
		gridLayout.horizontalSpacing = 0;
		gridLayout.verticalSpacing = 5;
		return gridLayout;
	}

}
