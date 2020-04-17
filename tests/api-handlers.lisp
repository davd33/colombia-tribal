(defpackage colombia-tribal/tests/api-handlers
  (:use :cl
        :api
        :rove))
(in-package :colombia-tribal/tests/api-handlers)

(defparameter cv-json1 "{
    \"title\": \"'( :david :rueda )\",
    \"subTitle\": \"Open your Mind and Technologies\",
    \"imageDescription\": \"me in an image\",
    \"contact\": {
        \"mail\": \"davd33@gmail.com\",
        \"linkedin\": \"https://www.linkedin.com/in/davd33-rueda/\",
        \"github\": \"https://www.github.com/davd33\"
    },
    \"workExperienceList\": [{
        \"title\": \"IT Consultant\",
        \"companyO\": \"Technology Partner\",
        \"description\": \"Working in Luxembourg for the Banque Internationale du Luxembourg as a Java and React developer.\",
        \"duration\": \"2019 - 10mo\",
        \"technologies\": \"Spring boot,Scala,React.js\"
    }, {
        \"title\": \"Entrepreneur\",
        \"description\": \"Development of an App for training German. A jump outside the comfort zone. Introduction to micro-services.\",
        \"duration\": \"2017 - 6mo\",
        \"technologies\": \"Angular 4,Node.js,AWS,Seneca.js\"
    }, {
        \"title\": \"Software Engineer\",
        \"companyO\": \"CGI Darmstadt\",
        \"refO\": \"https://drive.google.com/file/d/0B3Wu5re6rOM3aHV0THpPU1lrSGc/view?usp=sharing\",
        \"description\": \"Galileo - European Navigation System and ESA's satellite management software.\",
        \"duration\": \"2013-2017 - 3+y\",
        \"technologies\": \"Angular 4,Node.js,AWS\"
    }, {
        \"title\": \"Software Engineer\",
        \"companyO\": \"BIOTEC\",
        \"description\": \"Drug discovery for bone diseases: towards high-throughput and high-content phenotype comparison.\",
        \"duration\": \"2012-2013 - 9mo\",
        \"technologies\": \"Java,R,Swing,Weka ML,ImageJ\"
    }, {
        \"title\": \"Team Leader\",
        \"companyO\": \"Amerbank\",
        \"remoteO\": true,
        \"description\": \"Leading a development team for the creation of an internal AML facilities. A micro-service approach.\",
        \"duration\": \"2018 - 9mo\",
        \"technologies\": \"Istio,Kubernetes,AWS,Circle CI\"
    }],
    \"readingList\": [{
        \"title\": \"Practical Common Lisp\",
        \"image\": \"practical-common-lisp.cover.gif\",
        \"externalUrl\": \"http://www.gigamonkeys.com/book/\"
    }, {
        \"title\": \"Professional Clojure\",
        \"image\": \"professional-clojure.cover.jpg\",
        \"externalUrl\": \"https://www.oreilly.com/library/view/professional-clojure/9781119267270/\"
    }, {
        \"title\": \"Clojure for the brave and true\",
        \"image\": \"clojure-for-the-brave-and-true.cover.jpg\",
        \"externalUrl\": \"https://www.braveclojure.com/\"
    }],
    \"sectionList\": [{
        \"title\": \"my-experience-with-lisp\",
        \"paragraphList\": [{
            \"title\": \"1\",
            \"elementList\": [{
                \"order\": 1,
                \"content\": \"I have  been introduced to  functional programming end of  2018 as our  team was given the task to develop a new internal front-end application with React.js. We were coached by an external professional every two weeks.\"
            }]
        }, {
            \"title\": \"2\",
            \"elementList\": [{
                \"order\": 2,
                \"content\": \"My interest for functional programming grew and I started to \"
            }]
        }, {
            \"title\": \"2\",
            \"elementList\": [{
                \"order\": 3,
                \"content\": {
                    \"link\": \"http://learnyouahaskell.com/\",
                    \"text\": \"learn me a Haskell for a great good.\"
                }
            }]
        }, {
            \"title\": \"3\",
            \"elementList\": [{
                \"order\": 4,
                \"content\": \"Later on, a colleague  of mine told me about clojure so  passionately that I let Haskell aside and started learning  Clojure! I learned and practiced Clojure, reagent,  re-frame, compojure, ring (I might forget some). I made a  presentation about it at my company (\"
            }]
        }, {
            \"title\": \"3\",
            \"elementList\": [{
                \"order\": 5,
                \"content\": {
                    \"link\": \"https://github.com/davd33/cloj-tp\",
                    \"text\": \"the slides\"
                }
            }]
        }, {
            \"title\": \"3\",
            \"elementList\": [{
                \"order\": 6,
                \"content\": \" were a re-frame project  in order to demonstrate the fast feedback loop obtained by using the REPL with Figwheel). I went to the “heart  of clojure” in Leuven,  Belgium, met a lot  of interesting people there!\"
            }]
        }, {
            \"title\": \"4\",
            \"elementList\": [{
                \"order\": 7,
                \"content\": \"In order to deepen my understanding of Lisp, I started to learn Common-Lisp in August 2019. I created this web-page with the HTML generation library for CL: \"
            }]
        }, {
            \"title\": \"4\",
            \"elementList\": [{
                \"order\": 8,
                \"content\": {
                    \"link\": \"https://github.com/ruricolist/spinneret\",
                    \"text\": \"Spinneret\"
                }
            }]
        }, {
            \"title\": \"4\",
            \"elementList\": [{
                \"order\": 9,
                \"content\": \". It taught me a lot about how to manage with macros as well as how to use Quicklisp and much more.\"
            }]
        }]
    }, {
        \"title\": \"about.me.txt.p\",
        \"paragraphList\": [{
            \"title\": \"1\",
            \"elementList\": [{
                \"order\": 1,
                \"content\": \"Free software is Love.\"
            }]
        }, {
            \"title\": \"2\",
            \"elementList\": [{
                \"order\": 2,
                \"content\": \"For a better quality, for more security and for a better respect of human rights. It is the way to go if we want to have a chance to evolve our consciousness as a society and as individuals.\"
            }]
        }, {
            \"title\": \"3\",
            \"elementList\": [{
                \"order\": 3,
                \"content\": \"I've been working in different areas. From computational biology in bio-image recognition to the space industry with the automation of the Galileo satellite fleet and the maintenance of ESA's SCOS-2000 and now in Finance, I've been through different aspects of developing IT products.\"
            }]
        }, {
            \"title\": \"4\",
            \"elementList\": [{
                \"order\": 4,
                \"content\": \"Communication with transparency and benevolance is the absolute must for a healthy work collaboration.\"
            }]
        }, {
            \"title\": \"5\",
            \"elementList\": [{
                \"order\": 5,
                \"content\": \"And Lisp is the love of god.\"
            }]
        }]
    }]
}")

(deftest test-cv-handler
  (testing "cv can be created"
    (let* ((result (json:decode-json-from-string (cv-handler cv-json1)))
           (cv-id (jsons:get-in result :created :cv-id))
           (command (jsons:get-in result :command))
           (paragraphs (jsons:get-in result :created :paragraphs-elements))
           (we (jsons:get-in result :created :work-experiences))
           (readings (jsons:get-in result :created :readings)))
      (ok (numberp cv-id))
      (ok (string= command "create-cv"))
      (ok (and (numberp paragraphs)
               (= paragraphs 14)))
      (ok (and (numberp we)
               (= we 5)))
      (ok (and (numberp readings)
               (= readings 3))))))
