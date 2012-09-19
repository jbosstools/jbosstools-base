package org.jboss.tools.ui.bot.ext;

import java.awt.AWTException;
import java.io.File;
import java.io.IOException;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Map.Entry;
import java.util.Set;
import java.util.TreeSet;

import org.apache.log4j.Logger;
import org.eclipse.swtbot.swt.finder.junit.ScreenshotCaptureListener;
import org.jboss.tools.test.util.ScreenRecorderExt;
import org.jboss.tools.ui.bot.ext.config.Annotations.Require;
import org.jboss.tools.ui.bot.ext.config.TestConfiguration;
import org.jboss.tools.ui.bot.ext.config.TestConfigurator;
import org.jboss.tools.ui.bot.ext.config.requirement.RequirementBase;
import org.junit.Test;
import org.junit.experimental.categories.Categories.ExcludeCategory;
import org.junit.experimental.categories.Categories.IncludeCategory;
import org.junit.experimental.categories.Category;
import org.junit.runner.Description;
import org.junit.runner.Runner;
import org.junit.runner.manipulation.Filter;
import org.junit.runner.manipulation.NoTestsRemainException;
import org.junit.runner.notification.RunListener;
import org.junit.runner.notification.RunNotifier;
import org.junit.runners.BlockJUnit4ClassRunner;
import org.junit.runners.Suite;
import org.junit.runners.model.FrameworkMethod;
import org.junit.runners.model.InitializationError;
import org.junit.runners.model.RunnerBuilder;
import org.junit.runners.model.Statement;

/**
 * JUnit4 requirement aware testsuite runner. If suite class is annotated by @
 * RunWith({@link RequirementAwareSuite}) class, test classes can have
 * {@link Require} annotations
 * 
 * @author lzoubek@redhat.com
 */
public class RequirementAwareSuite extends Suite {
  private static boolean runManageBlockingWindow = true;
  private static ScreenRecorderExt screenRecorderExt = null;
	// we have one global instance of cleanup listener
	final static DoAfterAllTestsRunListener cleanUp = new DoAfterAllTestsRunListener();

	final Filter categoryFilter;

	public static class CategoryFilter extends Filter {
		public static CategoryFilter include(Class<?> categoryType) {
			return new CategoryFilter(categoryType, null);
		}

		private final Class<?> fIncluded;
		private final Class<?> fExcluded;

		public CategoryFilter(Class<?> includedCategory,
				Class<?> excludedCategory) {
			fIncluded = includedCategory;
			fExcluded = excludedCategory;
		}

		@Override
		public String describe() {
			return "category " + fIncluded;
		}

		@Override
		public boolean shouldRun(Description description) {
			if (hasCorrectCategoryAnnotation(description))
				return true;
			for (Description each : description.getChildren())
				if (shouldRun(each))
					return true;
			return false;
		}

		private boolean hasCorrectCategoryAnnotation(Description description) {
			List<Class<?>> categories = categories(description);
			if (categories.isEmpty())
				return fIncluded == null;
			for (Class<?> each : categories)
				if (fExcluded != null && fExcluded.isAssignableFrom(each))
					return false;
			for (Class<?> each : categories)
				if (fIncluded == null || fIncluded.isAssignableFrom(each))
					return true;
			return false;
		}

		private List<Class<?>> categories(Description description) {
			ArrayList<Class<?>> categories = new ArrayList<Class<?>>();
			categories.addAll(Arrays.asList(directCategories(description)));
			// categories.addAll(Arrays.asList(directCategories(parentDescription(description))));
			return categories;
		}

		private Description parentDescription(Description description) {
			// TODO: how heavy are we cringing?
			return Description.createSuiteDescription(description
					.getTestClass());
		}

		private Class<?>[] directCategories(Description description) {
			Category annotation = description.getAnnotation(Category.class);
			if (annotation == null)
				return new Class<?>[0];
			return annotation.value();
		}
	}

	class ReqAwareClassRunner extends BlockJUnit4ClassRunner {
		private final TestConfiguration config;
		private final List<RequirementBase> requirements;

		public ReqAwareClassRunner(Class<?> klass,
				List<RequirementBase> requirements, TestConfiguration config)
				throws InitializationError {
			super(klass);
			this.requirements = requirements;
			this.config = config;

			try {
				if (categoryFilter != null){
					filter(categoryFilter);					
				}
			} catch (NoTestsRemainException e) {
				// TODO Auto-generated catch block
				throw new InitializationError(e);
			}

		}
		

		public List<RequirementBase> getRequirements() {
			return Collections.unmodifiableList(this.requirements);
		}
		
		@Override
		protected List<FrameworkMethod> computeTestMethods() {
			List<FrameworkMethod> testMethods = new ArrayList<FrameworkMethod>();			
			for (Method mm : getTestClass().getJavaClass().getMethods()) {
				if (mm.getAnnotation(Test.class)!=null 
						||
						(mm.getName().startsWith("test") 
						&& !Modifier.isStatic(mm.getModifiers())
						&& mm.getParameterTypes().length==0
						&& Void.TYPE.equals(mm.getReturnType())
						)) {
					testMethods.add(new FrameworkMethod(mm));
				}
			}
			for (FrameworkMethod method : testMethods) {
				method.getAnnotation(Category.class);
			}
			return testMethods;
		}
		

		@Override
		public void run(RunNotifier notifier) {
			// planned test counter must know about all tests (methods) within a
			// class
			cleanUp.incrPlanned(getChildren().size() - 1);
			// ensure that we have exactly 1 cleanup listener registered
			notifier.removeListener(cleanUp);
			notifier.addListener(cleanUp);
			// adding ability to create screen shot (taken from
			// SWTBotJunit4ClassRunner)
			RunListener failureSpy = new ScreenshotCaptureListener();
			notifier.removeListener(failureSpy);
			notifier.addListener(failureSpy);
			try {
				super.run(notifier);
			} finally {
        if (System.getProperty("swt.bot.test.record.screencast","false").equalsIgnoreCase("true")){
		      RequirementAwareSuite.stopScreenRecorder();
		    }
				notifier.removeListener(failureSpy);
			}
		}

		@Override
		protected String testName(FrameworkMethod method) {
			return config.getPropName() + " - " + method.getName();
		}

		@Override
		protected Statement withBeforeClasses(Statement statement) {
		  if (System.getProperty("swt.bot.test.record.screencast","false").equalsIgnoreCase("true")){
		    RequirementAwareSuite.startScreenRecorder(getTestClass().getJavaClass().getSimpleName());
		  }
		  
		  if (SWTJBTExt.isRunningOnMacOs() && "1.7".equals(System.getProperty("java.specification.version"))){
			  System.setProperty("awt.toolkit", "sun.lwawt.macosx.LWCToolkit");
		  }
		  
		  if (RequirementAwareSuite.runManageBlockingWindow){
		    SWTJBTExt.manageBlockingWidows(false, false);
		    RequirementAwareSuite.runManageBlockingWindow = false;
		  }
			if (!this.config.equals(TestConfigurator.currentConfig)) {
				TestConfigurator.currentConfig = this.config;
			}
			log.info("Fullfilling requirements before test "
					+ getTestClass().getJavaClass());

			try {
				for (RequirementBase r : requirements) {
					r.fulfill();
				}
			} catch (Exception e) {
				log.error("Fulfilling failed", e);
			}
			log.info("Done");


			return super.withBeforeClasses(statement);
		}
	}
	

	private static final Logger log = Logger
			.getLogger(RequirementAwareSuite.class);

	private class RequirementAwareRunnerBuilder extends RunnerBuilder {
		private final TestConfiguration config;

		public RequirementAwareRunnerBuilder(TestConfiguration config) {
			this.config = config;
		}

		@Override
		public Runner runnerForClass(Class<?> klass) throws Throwable {
			if (!this.config.equals(TestConfigurator.currentConfig)) {
				TestConfigurator.currentConfig = this.config;
			}
			log.info("class " + klass.getCanonicalName());
			List<RequirementBase> reqs = TestConfigurator
					.getClassRequirements(klass);
			if (reqs != null) {
				if (cleanUp.isClassPlanned(klass)) {
					if (TestConfigurator.isRequiresRunOnce(klass)) {
						// class is already scheduled to run and contains
						// annotation runOnce
						log.info("runOnce=true, class already planned");
						log.info("Skipped");
						return null;
					}
					if (!TestConfigurator.isRequiresAnyRuntime(klass)) {
						// class is scheduled and does not require any runtime, thus
						// no need to run it against other configuration
						log.info("no runtimes required + class already planned");
						log.info("Skipped");
						return null;
					}
				}
				log.info("OK");
				// increment number of tests planned to run by 1 (class contains
				// at least 1 test method)
				cleanUp.incrPlanned();
				cleanUp.addClass(klass);
				return new ReqAwareClassRunner(klass, reqs, config);
			}
			log.info("Skipped");
			cleanUp.addSkippedClass(klass);
			return null;
		}

	}

	/**
	 * listener which listens to test runs, does some cleanup after all tests
	 * have run it also holds set of all classes which run (usefull for runOnce
	 * annotation)
	 * 
	 * @author lzoubek
	 * 
	 */
	static class DoAfterAllTestsRunListener extends RunListener {
		// As we can run more suites at once, we need to count tests which are
		// planned to run
		// and the ones which already passed (or failed), perform cleanups when
		// the last one finishes
		private int testsAboutToRun = 0;
		private int testsFinished = 0;

		public void incrPlanned() {
			testsAboutToRun += 1;
		}

		/**
		 * adds class to the list of skipped classes
		 * 
		 * @param klass
		 */
		public void addSkippedClass(Class<?> klass) {
			skippedClasses.add(klass.getName());

		}

		public void incrPlanned(int amount) {
			testsAboutToRun += amount;
		}

		public void incrFinished() {
			testsFinished += 1;
		}

		public int getPlanned() {
			return testsAboutToRun;
		}

		public int getFinished() {
			return testsFinished;
		}

		private Set<String> classes = new HashSet<String>();

		/**
		 * adds class to runList - as it is planned to run
		 * 
		 * @param klass
		 */
		public void addClass(Class<?> klass) {
			classes.add(klass.getName());
		}

		public boolean isClassPlanned(Class<?> klass) {
			return classes.contains(klass.getName());
		}

		/**
		 * set of classes that has been skipped (annotations not met etc)
		 */
		private Set<String> skippedClasses = new TreeSet<String>();

		private void reportSkippedClasses() {
			Set<String> finalized = new TreeSet<String>();
			// lets figure out if a class that has been at least once skipped
			// was not planned
			for (String clazz : skippedClasses) {
				if (!classes.contains(clazz)) {
					finalized.add(clazz);
				}
			}
			if (!finalized.isEmpty()) {
				log.info("Several test classes have been skipped, see head of log to figure out why it happened");
				for (String clazz : finalized) {
					log.info(" * " + clazz);
				}
			}
		}

		@Override
		public void testFinished(Description description) throws Exception {
			incrFinished();
			log.info("Finished test : " + description.getDisplayName());
			log.info("Finished tests : " + getFinished() + "/" + getPlanned());
			if (getFinished() >= getPlanned()) {
				log.info("All tests finished, performing cleanup requirements ");
				try {
					RequirementBase.createStopServer().fulfill();
					RequirementBase.createStopDBServer().fulfill();

					log.info("All cleanup requirements performed");
				} catch (Exception ex) {
					log.error("Unable to fulfill cleanup requirements", ex);
				}
				reportSkippedClasses();
			}
			super.testFinished(description);
		}
	}

	private final ArrayList<Runner> runners = new ArrayList<Runner>();

	/**
	 * Only called reflectively. Do not use programmatically.
	 */

	/**
	 * Called reflectively on classes annotated with
	 * <code>@RunWith(RequirementAwareSuite.class)</code>
	 * 
	 * @param klass
	 *            the root class
	 */
	public RequirementAwareSuite(Class<?> klass) throws Throwable {
		super(klass, Collections.<Runner> emptyList());
		log.info("Loading test configurations");

		for (Entry<Object, Object> entry : TestConfigurator.multiProperties
				.entrySet()) {
			try {
				TestConfiguration config = new TestConfiguration(entry.getKey()
						.toString(), entry.getValue().toString());
				String suiteName = config.getPropName() + " - "
						+ klass.getCanonicalName();
				log.info("Determine whether test classes meet configuration");
				NamedSuite suite = new NamedSuite(klass,new RequirementAwareRunnerBuilder(config), suiteName);
				// when no class mathces given config, do not init it 
				if (suite.getRunnerCount()>0) {
					log.info("Configuration '"+config.getPropName()+"' initialized with "+suite.getRunnerCount()+" runners.");
					runners.add(suite);
					config.initialize();
				}
				else {
					log.info("Configuration '"+config.getPropName()+"' skipped, no runners");				
				}
			} catch (Exception ex) {
				log.error("Error loading test configuration", ex);
				throw ex;
			}
		}

		try {
			categoryFilter = new CategoryFilter(getIncludedCategory(klass),
					getExcludedCategory(klass));
			filter(categoryFilter);

		} catch (NoTestsRemainException e) {
			throw new InitializationError(e);
		}
	}

	@Override
	protected List<Runner> getChildren() {
		return runners;
	}

	public class NamedSuite extends Suite {
		private final String suiteName;

		public NamedSuite(Class<?> klass, RunnerBuilder builder, String name)
				throws InitializationError {
			super(klass, builder);
			this.suiteName = name;
		}
		/**
		 * gets count of test runners within this suite
		 * @return
		 */
		public int getRunnerCount() {
			return getChildren().size();
		}

		@Override
		protected String getName() {
			return suiteName;
		}
	}

	private Class<?> getIncludedCategory(Class<?> klass) {
		IncludeCategory annotation = klass.getAnnotation(IncludeCategory.class);
		return annotation == null ? null : annotation.value();
	}

	private Class<?> getExcludedCategory(Class<?> klass) {
		ExcludeCategory annotation = klass.getAnnotation(ExcludeCategory.class);
		return annotation == null ? null : annotation.value();
	}
	/**
	 * Starts Screen Recorder
	 */
  private static void startScreenRecorder(String className) {
    if (screenRecorderExt == null) {
      try {
        screenRecorderExt = new ScreenRecorderExt();
      } catch (IOException ioe) {
        throw new RuntimeException("Unable to initialize Screen Recorder.", ioe);
      } catch (AWTException awte) {
        throw new RuntimeException("Unable to initialize Screen Recorder.", awte);
      }
    }
    if (screenRecorderExt != null) {
      if (screenRecorderExt.isState(ScreenRecorderExt.STATE_DONE)) {
        try {
          File screenCastDir = new File ("screencasts");
          if (!screenCastDir.exists()){
            screenCastDir.mkdir();
          }
          final String fileName = "screencasts" + File.separator + className;
          log.info("Starting Screen Recorder. Saving Screen Cast to file: " + fileName);
          screenRecorderExt.start(fileName);
        } catch (IOException ioe) {
          throw new RuntimeException("Unable to start Screen Recorder.", ioe);
        }
      } else {
        throw new RuntimeException(
            "Unable to start Screen Recorder.\nScreen Recorder is not in state DONE.");
      }
    } else {
      log.error("Screen Recorder was not properly initilized");
    }
  }
	/**
	 * Stops Screen Recorder
	 */
  private static void stopScreenRecorder(){
    if (screenRecorderExt != null){
      if (screenRecorderExt.isState(ScreenRecorderExt.STATE_RECORDING)){
        try {
          screenRecorderExt.stop();
          log.info("Screen Recorder stopped.");
        } catch (IOException ioe) {
          throw 
            new RuntimeException("Unable to stop Screen Recorder." , ioe);
        }
      }
      else{
        throw 
          new RuntimeException("Unable to stop Screen Recorder.\nScreen Recorder is no in state RECORDING.");
      }
    }
    else {
      throw 
        new RuntimeException("Unable to stop Screen Recorder.\nScreen Recorder was not properly initilized");
    }
  }
}
